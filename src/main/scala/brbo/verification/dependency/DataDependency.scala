package brbo.verification.dependency

import brbo.common.TargetMethod
import brbo.verification.dependency.reachdef.{ReachingStore, ReachingTransfer, ReachingValue}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.analysis.{AnalysisResult, ForwardAnalysisImpl}
import org.checkerframework.dataflow.cfg.node.Node

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

object DataDependency {
  private val logger = LogManager.getLogger("brbo.verification.dependency.DataDependency")

  private val transfer = new ReachingTransfer()
  private val forwardAnalysisImpl = new ForwardAnalysisImpl[ReachingValue, ReachingStore, ReachingTransfer](transfer)

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
}

