package brbo.boundinference.dependencyanalysis.reachingdefinition

import org.checkerframework.dataflow.analysis.AbstractValue
import org.checkerframework.dataflow.cfg.node.Node

case class ReachingValue(node: Option[Node], variable: String) extends AbstractValue[ReachingValue] {
  override def leastUpperBound(other: ReachingValue): ReachingValue = {
    ???
  }
}

