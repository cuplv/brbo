package brbo.verification.dependency

import brbo.common.{CFGUtils, TargetMethod}
import com.ibm.wala.util.graph.NumberedGraph
import com.ibm.wala.util.graph.dominators.Dominators
import org.checkerframework.dataflow.cfg.block.Block
import org.checkerframework.dataflow.cfg.node.Node

import scala.collection.JavaConverters._

class Dominator(targetMethod: TargetMethod) {
  private val (graph: NumberedGraph[BrboNode], root) = CFGUtils.deepCopyGraph(targetMethod.cfg, transpose = false)
  private val dominators = Dominators.make(graph, root)

  def isDominatedBy(node1: Node, node2: Node): Boolean = {
    dominators.isDominatedBy(getBrboNode(node1.getBlock), getBrboNode(node2.getBlock))
  }

  private def getBrboNode(block: Block): BrboNode = {
    graph.asScala.find(n => n.block == block) match {
      case Some(value) => value
      case None => throw new Exception("Unexpected")
    }
  }
}
