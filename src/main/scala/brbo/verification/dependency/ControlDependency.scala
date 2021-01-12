package brbo.verification.dependency

import brbo.common.TargetMethod
import com.ibm.wala.util.graph.NumberedGraph
import com.ibm.wala.util.graph.dominators.DominanceFrontiers
import com.ibm.wala.util.graph.impl.DelegatingNumberedGraph
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.block.Block

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

object ControlDependency {
  /**
   *
   * @param targetMethod Compute control dependency of each basic block, according to
   *                     Cytron, Ron, et al. "An efficient method of computing static
   *                     single assignment form." Proceedings of the 16th ACM SIGPLAN-SIGACT
   *                     symposium on Principles of programming languages. 1989.
   * @return A mapping from each block to the blocks that it control depend on
   */
  def computeControlDependency(targetMethod: TargetMethod): Map[Block, Set[Block]] = {
    val (graph: NumberedGraph[BrboNode], root) = deepCopyAndTransposeGraph(targetMethod.cfg)
    val dominanceFrontiers = new DominanceFrontiers(graph, root)

    var map = new HashMap[Block, Set[Block]]
    graph.asScala.foreach({
      brboNode =>
        val nodes: Iterator[BrboNode] = dominanceFrontiers.getDominanceFrontier(brboNode).asScala
        nodes.foreach({
          node =>
            val x: Set[Block] =
              map.get(node.block) match {
                case Some(y) => y
                case None => new HashSet[Block]
              }
            map = map + (brboNode.block -> (x + brboNode.block))
        })
    })
    map
  }

  private def deepCopyAndTransposeGraph(cfg: ControlFlowGraph): (NumberedGraph[BrboNode], BrboNode) = {
    var map = new HashMap[Block, BrboNode]

    def getNode(block: Block): BrboNode = {
      map.get(block) match {
        case Some(node) => node
        case None =>
          val node = BrboNode(block)
          map = map + (block -> node)
          node
      }
    }

    val graph = new DelegatingNumberedGraph[BrboNode]()

    var visited = new HashSet[Block]
    val stack = new java.util.Stack[Block]

    stack.push(cfg.getEntryBlock)
    while (!stack.empty()) {
      val top: Block = stack.pop()
      val topNode: BrboNode = getNode(top)

      if (!visited.contains(top)) {
        visited = visited + top
        // Add the node upon the 1st visit
        graph.addNode(topNode)
      }

      top.getSuccessors.asScala.foreach({
        block =>
          if (!visited.contains(block))
            stack.push(block)

          // Add the reversed edge
          val from = topNode
          val to = getNode(block)
          graph.addEdge(to, from)
      })
    }

    (graph, map(cfg.getRegularExitBlock))
  }
}
