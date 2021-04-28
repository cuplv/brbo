package brbo.verification.dependency

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.cfg.{CFGUtils, UniqueNode}
import brbo.common.{GhostVariableUtils, TargetMethod}
import brbo.verification.decomposition.TaintSet
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.block.Block.BlockType
import org.checkerframework.dataflow.cfg.block.{Block, ConditionalBlock}
import org.checkerframework.dataflow.cfg.node.Node

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

object DependencyAnalysis {
  private val logger = LogManager.getLogger("brbo.verification.decomposition.DecompositionUtils")

  def traceOrError(message: String, debug: Boolean): Unit = {
    if (debug) logger.error(message)
    else logger.trace(message)
  }

  /**
   *
   * @param node  A node
   * @param block A block that controls the enclosing block of the input node
   * @param debug Print debug information
   * @return The condition node that the block is control-dependent on
   */
  private def getCondition(node: Node, block: Block, debug: Boolean): Option[Node] = {
    def blocksToString(blocks: Iterable[Block]): String = {
      val body = blocks.map(block => s"${block.getUid}: $block").mkString("\n")
      s"====\n$body\n===="
    }

    assert(block.isInstanceOf[ConditionalBlock], s"Block ${node.getBlock.getUid} control depends on block ${block.getUid}")
    val predecessors = block.getPredecessors.asScala
    assert(predecessors.size == 1)

    val predecessor = predecessors.head
    val condition = predecessor.getLastNode
    if (condition == null) {
      traceOrError(s"${node.getBlock}'s predecessor block does not have a condition: `$predecessor`", debug)
      if (predecessor.getType == BlockType.EXCEPTION_BLOCK) {
        traceOrError(s"${node.getBlock}'s predecessor block is an exception block: `$predecessor`", debug)

        val predecessors2 = predecessor.getPredecessors.asScala
        assert(predecessors2.size == 1, s"\n${blocksToString(predecessors2)}")
        traceOrError(s"The last node (predecessor^2) in the exception block is: `${predecessors2.head.getLastNode}`", debug)

        // val predecessors3 = predecessors2.head.getPredecessors.asScala
        // assert(predecessors3.size == 1, s"\n${blocksToString(predecessors2)}\n${blocksToString(predecessors3)}")
        // traceOrError(s"The last node (predecessor^3) is: `${predecessors3.head.getLastNode}`", debug)

        // val predecessors4 = predecessors3.head.getPredecessors.asScala
        // assert(predecessors4.size == 1, s"\n${blocksToString(predecessors2)}\n${blocksToString(predecessors3)}\n${blocksToString(predecessors4)}")
        // traceOrError(s"The last node (predecessor^4) is: `${predecessors4.head.getLastNode}`", debug)

        Some(predecessors2.head.getLastNode)
      }
      else None
    }
    else Some(condition)
  }

  def controlDataDependencyForResources(targetMethod: TargetMethod, debug: Boolean): TaintSet = {
    val reachingDefinitions = targetMethod.reachingDefinitions
    val controlDependency = targetMethod.controlDependency
    val taintSets = targetMethod.cfg.getAllNodes.asScala
      .filter({ node => GhostVariableUtils.extractUpdate(node, Resource).isDefined })
      .map({ node =>
        traceOrError(s"Compute taint set for $node", debug)
        controlDataDependencyForNode(node, inputNodeIsExpression = false, reachingDefinitions, controlDependency, Set(), debug)
      })
      .toSet
    TaintSet.merge(taintSets)
  }

  def transitiveDataDependency(inputNode: Node,
                               reachingDefinitions: ReachingDefinition,
                               visited: Set[UniqueNode],
                               excludeResourceVariables: Boolean,
                               debug: Boolean): TaintSet = {
    if (visited.contains(UniqueNode(inputNode))) return TaintSet(new HashSet[String], new HashSet[String])

    val usedVariables: Set[String] = {
      val usedVariables = CFGUtils.getUsedVariables(inputNode, isExpression = false)
      traceOrError(s"Used variables: $usedVariables", debug)
      if (excludeResourceVariables)
        usedVariables.filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource))
      else
        usedVariables
    }

    val set = reachingDefinitions.get(inputNode) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        traceOrError(s"Reaching definitions: $definitions. Used: $newDefinitions", debug)
        val taintSets = newDefinitions.map({
          definition =>
            definition.node match {
              case Some(UniqueNode(node)) => transitiveDataDependency(node, reachingDefinitions, visited + UniqueNode(inputNode), excludeResourceVariables, debug)
              case None => TaintSet(HashSet[String](definition.variable), HashSet[String](definition.variable)) // This definition comes from an input variable
            }
        })
        TaintSet.merge(taintSets)
      case None => TaintSet(new HashSet[String], new HashSet[String])
    }
    TaintSet(usedVariables ++ set.allVariables, set.inputs)
  }

  private def controlDataDependencyForNode(inputNode: Node,
                                           inputNodeIsExpression: Boolean,
                                           reachingDefinitions: ReachingDefinition,
                                           controlDependency: Map[Block, Set[Block]],
                                           visited: Set[UniqueNode],
                                           debug: Boolean): TaintSet = {
    if (visited.contains(UniqueNode(inputNode))) return TaintSet(new HashSet[String], new HashSet[String])
    traceOrError(s"Visit node `$inputNode`", debug)

    val usedVariables: Set[String] = CFGUtils.getUsedVariables(inputNode, inputNodeIsExpression)
    traceOrError(s"Used variables: $usedVariables", debug)

    val set1: TaintSet = reachingDefinitions.get(inputNode) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        traceOrError(s"Reaching definitions: $definitions. Used: $newDefinitions", debug)
        val taintSets = newDefinitions.map({
          definition =>
            definition.node match {
              case Some(UniqueNode(node)) =>
                controlDataDependencyForNode(node, inputNodeIsExpression = false,
                  reachingDefinitions, controlDependency, visited + UniqueNode(inputNode), debug)
              case None => TaintSet(HashSet[String](definition.variable), HashSet[String](definition.variable)) // This definition comes from an input variable
            }
        })
        TaintSet.merge(taintSets)
      case None => TaintSet(new HashSet[String], new HashSet[String])
    }

    val set2: TaintSet = controlDependency.get(inputNode.getBlock) match {
      case Some(blocks) =>
        val taintSets = blocks.map({
          block =>
            traceOrError(s"Compute taint set for conditional node $inputNode", debug)
            getCondition(inputNode, block, debug) match {
              case Some(condition) =>
                controlDataDependencyForNode(condition, inputNodeIsExpression = true,
                  reachingDefinitions, controlDependency, visited + UniqueNode(inputNode), debug)
              case None => TaintSet(new HashSet[String], new HashSet[String])
            }
        })
        TaintSet.merge(taintSets)
      case None => TaintSet(new HashSet[String], new HashSet[String])
    }

    TaintSet(usedVariables ++ set1.allVariables ++ set2.allVariables, set1.inputs ++ set2.inputs)
  }

  def taintSetPerExecution(resourceUpdateNode: Node,
                           reachingDefinitions: ReachingDefinition,
                           controlDependency: Map[Block, Set[Block]],
                           debug: Boolean): TaintSet = {
    assert(GhostVariableUtils.extractUpdate(resourceUpdateNode, Resource).isDefined)
    val usedVariables: Set[String] = CFGUtils.getUsedVariables(resourceUpdateNode, isExpression = false).filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource))
    val set = reachingDefinitions.get(resourceUpdateNode) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        traceOrError(s"Reaching definitions (that are used): $newDefinitions", debug)
        val taintSets = newDefinitions.map({
          definition =>
            definition.node match {
              case Some(UniqueNode(node)) => controlDataDependencyForNode(node, inputNodeIsExpression = false, reachingDefinitions, controlDependency, Set(UniqueNode(resourceUpdateNode)), debug)
              case None => TaintSet(HashSet[String](definition.variable), HashSet[String](definition.variable)) // This definition comes from an input variable
            }
        })
        TaintSet.merge(taintSets)
      case None => TaintSet(new HashSet[String], new HashSet[String])
    }
    TaintSet(usedVariables ++ set.allVariables, set.inputs)
  }
}
