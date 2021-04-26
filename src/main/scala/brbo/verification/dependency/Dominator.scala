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
    val block1 = node1.getBlock
    val block2 = node2.getBlock
    if (block1 != block2) {
      dominators.isDominatedBy(getBrboNode(block1), getBrboNode(block2))
    }
    else {
      val index1 = block1.getNodes.indexOf(node1)
      val index2 = block1.getNodes.indexOf(node2)
      assert(index1 != -1 && index2 != -1)
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
