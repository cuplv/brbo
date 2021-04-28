package brbo.verification.dependency.reachdef

import brbo.common.cfg.UniqueNode
import org.checkerframework.dataflow.analysis.AbstractValue

case class ReachingValue(node: Option[UniqueNode], variable: String) extends AbstractValue[ReachingValue] {
  override def leastUpperBound(other: ReachingValue): ReachingValue = {
    ???
  }

  override def toString: String = {
    node match {
      case Some(n) => s"`$variable` in `${node.get}`"
      case None => s"`$variable` (input)"
    }
  }
}

