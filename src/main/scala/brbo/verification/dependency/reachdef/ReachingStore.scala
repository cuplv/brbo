package brbo.verification.dependency.reachdef

import org.checkerframework.dataflow.analysis.Store
import org.checkerframework.dataflow.cfg.visualize.CFGVisualizer
import org.checkerframework.dataflow.expression.JavaExpression

case class ReachingStore(definitions: Set[ReachingValue]) extends Store[ReachingStore] {
  override def widenedUpperBound(previous: ReachingStore): ReachingStore = {
    ???
  }

  override def leastUpperBound(other: ReachingStore): ReachingStore = {
    ReachingStore(definitions ++ other.definitions)
  }

  override def copy(): ReachingStore = {
    ReachingStore(definitions)
  }

  override def canAlias(a: JavaExpression, b: JavaExpression): Boolean = true

  override def visualize(viz: CFGVisualizer[_, ReachingStore, _]): String = {
    ???
  }
}
