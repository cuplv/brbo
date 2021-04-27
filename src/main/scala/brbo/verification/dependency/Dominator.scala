package brbo.verification.dependency

import brbo.common.TargetMethod
import brbo.common.cfg.{CFGUtils, UniqueNode}
import com.ibm.wala.util.graph.NumberedGraph
import com.ibm.wala.util.graph.dominators.Dominators
import org.checkerframework.dataflow.cfg.block.Block

import scala.collection.JavaConverters._

class Dominator(targetMethod: TargetMethod) {
  private val (graph: NumberedGraph[BrboNode], root) = CFGUtils.deepCopyGraph(targetMethod.cfg, transpose = false)
  private val dominators = Dominators.make(graph, root)

  def isDominatedBy(node1: UniqueNode, node2: UniqueNode): Boolean = {
    val block1 = node1.node.getBlock
    val block2 = node2.node.getBlock
    if (block1 != block2) {
      dominators.isDominatedBy(getBrboNode(block1), getBrboNode(block2))
    }
    else {
      val index1 = CFGUtils.getNodeIndexInBlock(node1.node)
      val index2 = CFGUtils.getNodeIndexInBlock(node2.node)
      index1 >= index2
    }
  }

  private def getBrboNode(block: Block): BrboNode = {
    graph.asScala.find(n => n.block == block) match {
      case Some(value) => value
      case None => throw new Exception("Unexpected")
    }
  }
}
