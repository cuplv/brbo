package brbo.verification

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.{CFGUtils, GhostVariableUtils, TargetMethod, TreeUtils}
import brbo.verification.Decomposition.Subprogram
import brbo.verification.dependency.reachdef.ReachingValue
import brbo.verification.dependency.{ControlDependency, DataDependency}
import com.sun.source.tree.{LiteralTree, Tree}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.block.{Block, ConditionalBlock}
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node}

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

class Decomposition(inputMethod: TargetMethod) {
  private val logger = LogManager.getLogger(classOf[Decomposition])

  def decompose(): String = {
    var map = TreeUtils.collectCommands(inputMethod.methodTree.getBody).foldLeft(new HashMap[Tree, Subprogram])({
      (acc, statement) =>
        GhostVariableUtils.extractGhostVariableUpdate(statement, Resource) match {
          case Some(updateNode) =>
            updateNode.update match {
              case _: LiteralTree =>
                // The initial subprogram is the minimal enclosing loop
                val subprogram = inputMethod.getMinimalEnclosingLoop(statement)
                logger.error(s"Resource update `$statement`'s initial subprogram is `$subprogram`")
                acc + (statement -> Subprogram(Seq(subprogram)))
              case _ => acc + (statement -> Subprogram(Seq(statement)))
            }
          case None => acc
        }
    })
    map.toList.zip(map.toList).filter({
      case ((update1, subprogram1), (update2, subprogram2)) =>
        if (interfere(subprogram1, subprogram2)) {
          ???
        }
        else false
    })
    ???
  }

  def computeTaintSet(subprogram: Subprogram): Set[String] = {
    val targetMethod = BasicProcessor.getTargetMethod(inputMethod.className, extractSubprogram(subprogram))
    val dataDependency = DataDependency.computeDataDependency(targetMethod)
    val controlDependency = ControlDependency.computeControlDependency(targetMethod)

    ???
  }

  def computeModifiedSet(subprogram: Subprogram): Set[String] = {
    val targetMethod = BasicProcessor.getTargetMethod(inputMethod.className, extractSubprogram(subprogram))
    Decomposition.computeModifiedSet(targetMethod)
  }

  def extractSubprogram(subprogram: Subprogram): String = {
    ???
  }

  /**
   *
   * @param subprogram
   * @return A new subprogram s.t. it does not interfere with itself
   */
  def enlarge(subprogram: Subprogram): String = {
    ???
  }

  def merge(subprogram1: Subprogram, subprogram2: Subprogram): String = {
    ???
  }

  def environmentModifiedSet(subprograms: Iterable[Subprogram]): Set[String] = {
    ???
  }

  def interferedByEnvironment(subprogram: Subprogram, subprograms: Iterable[Subprogram]): Boolean = {
    ???
  }

  /**
   *
   * @param subprogram1
   * @param subprogram2
   * @return If subprogram1 may interfere with subprogram2's resource usage
   */
  def interfere(subprogram1: Subprogram, subprogram2: Subprogram): Boolean = {
    // TODO: Check if there exists a path from subprogram1's exit to subprogram2's entry
    val modifiedSet1 = computeModifiedSet(subprogram1)
    val taintSet1 = computeTaintSet(subprogram1)
    val modifiedSet2 = computeModifiedSet(subprogram2)
    val taintSet2 = computeTaintSet(subprogram2)
    modifiedSet1.intersect(taintSet2).nonEmpty || modifiedSet2.intersect(taintSet1).nonEmpty
  }
}

object Decomposition {
  private val logger = LogManager.getLogger("brbo.boundinference.dependency.Decomposition")

  def computeModifiedSet(targetMethod: TargetMethod): Set[String] = {
    targetMethod.cfg.getAllNodes.asScala.foldLeft(new HashSet[String])({
      (acc, node) =>
        node match {
          case assignmentNode: AssignmentNode => acc + assignmentNode.getTarget.toString
          case _ => acc
        }
    })
  }

  def computeTaintSetControlAndData(targetMethod: TargetMethod): Set[String] = {
    val dataDependency = DataDependency.computeDataDependency(targetMethod)
    val controlDependency = {
      val controlDependency = ControlDependency.computeControlDependency(targetMethod)
      ControlDependency.reverseControlDependency(controlDependency)
    }
    targetMethod.cfg.getAllNodes.asScala
      .filter({ node => GhostVariableUtils.extractGhostVariableUpdate(node, Resource).isDefined })
      .flatMap({ node =>
        logger.debug(s"Compute taint set for $node")
        computeTaintSetDataHelper(node, dataDependency, controlDependency, new HashSet[Node])
      })
      .toSet
  }

  private def computeTaintSetDataHelper(node: Node,
                                        dataDependency: Map[Node, Set[ReachingValue]],
                                        controlDependency: Map[Block, Set[Block]],
                                        visited: Set[Node]): Set[String] = {
    if (visited.contains(node)) return new HashSet[String]

    val assignmentExpression = node match {
      case assignmentNode: AssignmentNode => assignmentNode.getExpression
      case _ => throw new Exception(s"Compute taint set - Expecting assignment node $node (Type: ${node.getClass})")
    }

    val set1: Set[String] = dataDependency.get(node) match {
      case Some(definitions) =>
        val usedVariables = CFGUtils.getUsedVariables(assignmentExpression)
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        usedVariables ++ computeTaintSetHelper(newDefinitions, dataDependency, controlDependency, visited + node)
      case None => new HashSet[String]
    }

    val set2: Set[String] = controlDependency.get(node.getBlock) match {
      case Some(blocks) =>
        blocks.flatMap({
          block =>
            assert(block.isInstanceOf[ConditionalBlock], s"Block ${node.getBlock.getUid} control depends on block ${block.getUid}")
            val predecessors = block.getPredecessors.asScala
            assert(predecessors.size == 1)

            val condition = predecessors.head.getLastNode
            logger.debug(s"Compute taint set for conditional node $node")
            computeTaintSetControlHelper(condition, dataDependency, controlDependency, visited + node)
        })
      case None => new HashSet[String]
    }

    set1 ++ set2
  }

  private def computeTaintSetControlHelper(node: Node,
                                           dataDependency: Map[Node, Set[ReachingValue]],
                                           controlDependency: Map[Block, Set[Block]],
                                           visited: Set[Node]): Set[String] = {
    if (visited.contains(node)) return new HashSet[String]

    val usedVariables = CFGUtils.getUsedVariables(node)

    val set1 = dataDependency.get(node) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        usedVariables ++ computeTaintSetHelper(newDefinitions, dataDependency, controlDependency, visited + node)
      case None => new HashSet[String]
    }

    val set2 = controlDependency.get(node.getBlock) match {
      case Some(blocks) =>
        blocks.flatMap({
          block =>
            assert(block.isInstanceOf[ConditionalBlock], s"Block ${node.getBlock.getUid} control depends on block ${block.getUid}")
            val predecessors = block.getPredecessors.asScala
            assert(predecessors.size == 1)

            val condition = predecessors.head.getLastNode
            computeTaintSetControlHelper(condition, dataDependency, controlDependency, visited + node)
        })
      case None => new HashSet[String]
    }

    set1 ++ set2
  }

  private def computeTaintSetHelper(definitions: Iterable[ReachingValue],
                                    dataDependency: Map[Node, Set[ReachingValue]],
                                    controlDependency: Map[Block, Set[Block]],
                                    visited: Set[Node]): Set[String] = {
    definitions.flatMap({
      definition =>
        definition.node match {
          case Some(n) =>
            logger.debug(s"Compute taint set - Recursive call to $n")
            computeTaintSetDataHelper(n, dataDependency, controlDependency, visited)
          case None => HashSet[String](definition.variable) // This definition comes from an input variable
        }
    }).toSet
  }

  case class Subprogram(trees: Seq[Tree])
}