package brbo.verification.dependency.reachdef

import org.checkerframework.dataflow.analysis.AbstractValue
import org.checkerframework.dataflow.cfg.node.Node

case class ReachingValue(node: Option[Node], variable: String) extends AbstractValue[ReachingValue] {
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

