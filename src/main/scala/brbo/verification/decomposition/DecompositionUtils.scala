package brbo.verification.decomposition

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.{CFGUtils, GhostVariableUtils, TargetMethod}
import brbo.verification.dependency.reachdef.ReachingValue
import brbo.verification.dependency.{ControlDependency, ReachingDefinition}
import com.sun.source.tree.{ExpressionStatementTree, StatementTree}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.block.Block.BlockType
import org.checkerframework.dataflow.cfg.block.{Block, ConditionalBlock}
import org.checkerframework.dataflow.cfg.node._

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

object DecompositionUtils {

  private val logger = LogManager.getLogger("brbo.verification.decomposition.DecompositionUtils")

  def traceOrError(message: String, debug: Boolean): Unit = {
    if (debug) logger.error(message)
    else logger.trace(message)
  }

  /**
   *
   * @param statement    The statement from which we generate an initial subprogram
   * @param targetMethod The method that encloses the above statement
   * @return The initial program and its entry node in the CFG
   */
  def initializeGroups(statement: StatementTree, targetMethod: TargetMethod): Option[(StatementTree, Node)] = {
    statement match {
      case expressionStatementTree: ExpressionStatementTree =>
        GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource) match {
          case Some(updateTree) =>
            val updateNode = CFGUtils.getNodesForExpressionStatementTree(expressionStatementTree, targetMethod.cfg)

            updateTree.increment match {
              /*case literalTree: LiteralTree =>
                assert(literalTree.getKind == Kind.INT_LITERAL)
                // The initial subprogram is the minimal enclosing loop when `R` is updated by a constant
                val subprogram: StatementTree = {
                  TreeUtils.getMinimalEnclosingLoop(targetMethod.getPath(statement)) match {
                    case Some(enclosingLoop) => enclosingLoop
                    case None =>
                      logger.trace(s"Resource update `$statement` does not have an enclosing loop")
                      statement
                  }
                }.asInstanceOf[StatementTree]
                val entryNode: Node = CFGUtils.entryOfMinimalEnclosingLoop(updateNode, targetMethod) match {
                  case Some(entryNode) => entryNode
                  case None => updateNode
                }
                logger.trace(s"Resource update `$statement`'s initial subprogram is `$subprogram`. Entry node is `$entryNode`")
                Some(subprogram, entryNode)*/
              case _ => Some(statement, updateNode)
            }
          case None => None
        }
      case _ => None
    }
  }

  def computeModifiedSet(targetMethod: TargetMethod): Set[String] = {
    targetMethod.cfg.getAllNodes.asScala
      .foldLeft(new HashSet[String])({
        (acc, node) =>
          node match {
            case assignmentNode: AssignmentNode => acc + assignmentNode.getTarget.toString
            case _ => acc
          }
      })
      .filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource))
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
    val reachingDefinitions = ReachingDefinition.run(targetMethod)
    val controlDependency = {
      val controlDependency = ControlDependency.computeControlDependency(targetMethod)
      ControlDependency.reverseControlDependency(controlDependency)
    }
    val taintSets = targetMethod.cfg.getAllNodes.asScala
      .filter({ node => GhostVariableUtils.extractUpdate(node, Resource).isDefined })
      .map({ node =>
        traceOrError(s"Compute taint set for $node", debug)
        controlDataDependencyForNode(node, inputNodeIsExpression = false, reachingDefinitions, controlDependency, new HashSet[Node], debug)
      })
      .toSet
    TaintSet(taintSets.flatMap(set => set.allVariables), taintSets.flatMap(set => set.inputs))
  }

  private def controlDataDependencyForNode(inputNode: Node,
                                           inputNodeIsExpression: Boolean,
                                           reachingDefinitions: ReachingDefinition,
                                           controlDependency: Map[Block, Set[Block]],
                                           visited: Set[Node],
                                           debug: Boolean): TaintSet = {
    if (visited.contains(inputNode)) return TaintSet(new HashSet[String], new HashSet[String])

    val usedVariables: Set[String] = CFGUtils.getUsedVariables(inputNode, inputNodeIsExpression)
    traceOrError(s"Used variables: $usedVariables", debug)

    val set1: TaintSet = reachingDefinitions.get(inputNode) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        traceOrError(s"Reaching definitions (that are used): $newDefinitions", debug)
        val taintSets = newDefinitions.map({
          definition =>
            definition.node match {
              case Some(node) => controlDataDependencyForNode(node, inputNodeIsExpression = false, reachingDefinitions, controlDependency, visited + inputNode, debug)
              case None => TaintSet(HashSet[String](definition.variable), HashSet[String](definition.variable)) // This definition comes from an input variable
            }
        })
        TaintSet(taintSets.flatMap(set => set.allVariables), taintSets.flatMap(set => set.inputs))
      case None => TaintSet(new HashSet[String], new HashSet[String])
    }

    val set2: TaintSet = controlDependency.get(inputNode.getBlock) match {
      case Some(blocks) =>
        val taintSets = blocks.map({
          block =>
            traceOrError(s"Compute taint set for conditional node $inputNode", debug)
            getCondition(inputNode, block, debug) match {
              case Some(condition) => controlDataDependencyForNode(condition, inputNodeIsExpression = true, reachingDefinitions, controlDependency, visited + inputNode, debug)
              case None => TaintSet(new HashSet[String], new HashSet[String])
            }
        })
        TaintSet(taintSets.flatMap(set => set.allVariables), taintSets.flatMap(set => set.inputs))
      case None => TaintSet(new HashSet[String], new HashSet[String])
    }

    TaintSet(usedVariables ++ set1.allVariables ++ set2.allVariables, set1.inputs ++ set2.inputs)
  }

  def taintSetPerExecution(resourceUpdateNode: Node,
                           reachingDefinitions: ReachingDefinition,
                           controlDependency: Map[Block, Set[Block]],
                           visited: Set[Node],
                           debug: Boolean): TaintSet = {
    assert(GhostVariableUtils.extractUpdate(resourceUpdateNode, Resource).isDefined)
    val usedVariables: Set[String] = CFGUtils.getUsedVariables(resourceUpdateNode, isExpression = false).filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource))
    reachingDefinitions.get(resourceUpdateNode) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        traceOrError(s"Reaching definitions (that are used): $newDefinitions", debug)
        val taintSets = newDefinitions.map({
          definition =>
            definition.node match {
              case Some(node) => controlDataDependencyForNode(node, inputNodeIsExpression = false, reachingDefinitions, controlDependency, visited + resourceUpdateNode, debug)
              case None => TaintSet(HashSet[String](definition.variable), HashSet[String](definition.variable)) // This definition comes from an input variable
            }
        })
        TaintSet(usedVariables ++ taintSets.flatMap(set => set.allVariables), taintSets.flatMap(set => set.inputs))
      case None => TaintSet(usedVariables, new HashSet[String])
    }
  }
}

/**
 *
 * @param allVariables Local and input variables that taint resource updates
 * @param inputs       Input variables that taint resource updates
 */
case class TaintSet(allVariables: Set[String], inputs: Set[String])