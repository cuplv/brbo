package brbo.verification.dependency

import brbo.common.TargetMethod
import brbo.verification.dependency.reachdef.{ReachingStore, ReachingTransfer, ReachingValue}
import com.sun.source.tree.Tree
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.analysis.{AnalysisResult, ForwardAnalysisImpl}
import org.checkerframework.dataflow.cfg.node.Node

object ReachingDefinition {
  private val logger = LogManager.getLogger("brbo.verification.dependency.ReachingDefinition")

  private val transfer = new ReachingTransfer()
  private val forwardAnalysisImpl = new ForwardAnalysisImpl[ReachingValue, ReachingStore, ReachingTransfer](transfer)

  /**
   *
   * @return For each node, what are the definitions that can reach it (before it is executed)
   */
  def run(targetMethod: TargetMethod): ReachingDefinition = {
    val cfg = targetMethod.cfg
    forwardAnalysisImpl.performAnalysis(cfg)
    val result: AnalysisResult[ReachingValue, ReachingStore] = forwardAnalysisImpl.getResult
    new ReachingDefinition(result)
  }
}

class ReachingDefinition(result: AnalysisResult[ReachingValue, ReachingStore]) {
  def get(node: Node): Option[Set[ReachingValue]] = {
    val store = result.getStoreBefore(node)
    if (store == null) None
    else Some(store.definitions)
  }

  def get(tree: Tree): Option[Set[ReachingValue]] = {
    val store = result.getStoreBefore(tree)
    if (store == null) None
    else Some(store.definitions)
  }
}