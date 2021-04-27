package brbo.common.cfg

import org.checkerframework.dataflow.cfg.node.Node

case class UniqueNode(node: Node) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: UniqueNode => CFGUtils.nodeUniqueIdentifier(node) == CFGUtils.nodeUniqueIdentifier(other.node)
      case _ => false
    }
  }

  override def hashCode(): Int = {
    CFGUtils.nodeUniqueIdentifier(node).hashCode
  }
}
