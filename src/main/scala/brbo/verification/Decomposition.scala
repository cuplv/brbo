package brbo.verification

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.{CFGUtils, GhostVariableUtils, TargetMethod}
import brbo.verification.dependency.reachdef.ReachingValue
import brbo.verification.dependency.{ControlDependency, DataDependency}
import com.sun.source.tree.Tree
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.block.{Block, ConditionalBlock}
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node}

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

class Decomposition(inputMethod: TargetMethod) {
  private val logger = LogManager.getLogger(classOf[Decomposition])

  def computeTaintSet(location: Iterable[Tree]): Set[String] = {
    val targetMethod = BasicProcessor.getTargetMethod(inputMethod.className, extractSubprogram(location))
    val dataDependency = DataDependency.computeDataDependency(targetMethod)
    val controlDependency = ControlDependency.computeControlDependency(targetMethod)

    ???
  }

  def computeModifiedSet(location: Iterable[Tree]): Set[String] = {
    ???
  }

  def extractSubprogram(location: Iterable[Tree]): String = {
    ???
  }
}

object Decomposition {
  private val logger = LogManager.getLogger("brbo.boundinference.dependency.Decomposition")

  def computeTaintSet(targetMethod: TargetMethod): Set[String] = {
    val dataDependency = DataDependency.computeDataDependency(targetMethod)
    val controlDependency = {
      val controlDependency = ControlDependency.computeControlDependency(targetMethod)
      ControlDependency.reverseControlDependency(controlDependency)
    }
    targetMethod.cfg.getAllNodes.asScala
      .filter({ node => GhostVariableUtils.extractGhostVariableUpdate(node, Resource).isDefined })
      .flatMap({ node =>
        logger.error(s"Compute taint set for $node")
        computeTaintSetHelper(node, dataDependency, controlDependency, new HashSet[Node])
      })
      .toSet
  }

  private def computeTaintSetHelper(node: Node,
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
        usedVariables ++ computeTaintSetHelperHelper(newDefinitions, dataDependency, controlDependency, visited + node)
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
            val usedVariables = CFGUtils.getUsedVariables(condition)
            logger.error(s"Compute taint set for conditional node $condition (Used variables: $usedVariables)")

            dataDependency.get(condition) match {
              case Some(definitions) =>
                val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
                usedVariables ++ computeTaintSetHelperHelper(newDefinitions, dataDependency, controlDependency, visited + node)
              case None => new HashSet[String]
            }
        })
      case None => new HashSet[String]
    }

    set1 ++ set2
  }

  private def computeTaintSetHelperHelper(definitions: Iterable[ReachingValue],
                                          dataDependency: Map[Node, Set[ReachingValue]],
                                          controlDependency: Map[Block, Set[Block]],
                                          visited: Set[Node]): Set[String] = {
    definitions.flatMap({
      definition =>
        definition.node match {
          case Some(n) =>
            logger.error(s"Compute taint set - Recursive call to $n")
            computeTaintSetHelper(n, dataDependency, controlDependency, visited)
          case None => HashSet[String](definition.variable) // This definition comes from an input variable
        }
    }).toSet
  }
}