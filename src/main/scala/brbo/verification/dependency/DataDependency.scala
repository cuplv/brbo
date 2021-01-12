package brbo.verification.dependency

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.{CFGUtils, GhostVariableUtils, TargetMethod}
import brbo.verification.dependency.reachdef.{ReachingStore, ReachingTransfer, ReachingValue}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.analysis.{AnalysisResult, ForwardAnalysisImpl}
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node}

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

object DataDependency {
  private val logger = LogManager.getLogger("brbo.boundinference.dependency.DataDependency")

  private val transfer = new ReachingTransfer()
  private val forwardAnalysisImpl = new ForwardAnalysisImpl[ReachingValue, ReachingStore, ReachingTransfer](transfer)

  /**
   *
   * @param targetMethod
   * @return The variables that any command `r+=e` data-depends on
   */
  @deprecated
  def computeDependencySet(targetMethod: TargetMethod): Set[String] = {
    val cfg = targetMethod.cfg
    forwardAnalysisImpl.performAnalysis(cfg)
    val result: AnalysisResult[ReachingValue, ReachingStore] = forwardAnalysisImpl.getResult

    cfg.getAllNodes.asScala.flatMap({
      node =>
        GhostVariableUtils.extractGhostVariableUpdate(node, Resource) match {
          case Some(_) => getDataDependency(node, result, new HashSet[Node])
          case None => new HashSet[String]
        }
    }).toSet.intersect(targetMethod.inputVariables.keySet)
  }

  /**
   *
   * @param targetMethod
   * @return For each node, what are the definitions that can reach it (before it is executed)
   */
  def computeDataDependency(targetMethod: TargetMethod): Map[Node, Set[ReachingValue]] = {
    val cfg = targetMethod.cfg
    forwardAnalysisImpl.performAnalysis(cfg)
    val result: AnalysisResult[ReachingValue, ReachingStore] = forwardAnalysisImpl.getResult

    cfg.getAllNodes.asScala.foldLeft(new HashMap[Node, Set[ReachingValue]])({
      (acc, node) =>
        acc + (node -> result.getStoreBefore(node).definitions)
    })
  }

  private def getDataDependency(node: Node,
                                results: AnalysisResult[ReachingValue, ReachingStore],
                                visitedNodes: Set[Node]): Set[String] = {
    if (visitedNodes.contains(node)) return new HashSet[String]

    node match {
      case assignmentNode: AssignmentNode =>
        val reachingDefinitions = results.getStoreBefore(node).definitions
        logger.debug(s"Reaching definitions: $reachingDefinitions")

        val usedReachingDefinitions = {
          val usedVariablesInUpdate = CFGUtils.getUsedVariables(assignmentNode.getExpression)
          reachingDefinitions.filter({ reachingDefinition => usedVariablesInUpdate.contains(reachingDefinition.variable) })
        }
        val usedReachingDefinitionsVariables = usedReachingDefinitions.map(reachingDefinition => reachingDefinition.variable)
        logger.debug(s"Used reaching definitions: $usedReachingDefinitionsVariables")

        usedReachingDefinitionsVariables ++ usedReachingDefinitions.map(reachingDefinition => reachingDefinition.node).flatMap({
          case Some(node2) =>
            logger.debug(s"Recursive call on node $node2")
            val result = getDataDependency(node2, results, visitedNodes + node)
            logger.debug(s"Recursive call result is $result")
            result
          case None => new HashSet[String]
        })
      case _ =>
        logger.debug(s"Node $node is not an assignment node")
        new HashSet[String]
    }
  }
}
