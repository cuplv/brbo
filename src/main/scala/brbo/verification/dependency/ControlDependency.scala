package brbo.verification.dependency

import brbo.common.{CFGUtils, TargetMethod}
import com.ibm.wala.util.graph.NumberedGraph
import com.ibm.wala.util.graph.dominators.DominanceFrontiers
import org.checkerframework.dataflow.cfg.block.Block

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

object ControlDependency {
  /**
   *
   * @param targetMethod Compute control dependency, according to
   *                     Cytron, Ron, et al. "An efficient method of computing static
   *                     single assignment form." Proceedings of the 16th ACM SIGPLAN-SIGACT
   *                     symposium on Principles of programming languages. 1989.
   * @return A mapping from each block to the blocks that are dependent on it
   */
  def computeControlDependency(targetMethod: TargetMethod): Map[Block, Set[Block]] = {
    val (graph: NumberedGraph[BrboNode], root) = CFGUtils.deepCopyGraph(targetMethod.cfg, transpose = true)
    val dominanceFrontiers = new DominanceFrontiers(graph, root)

    var map = targetMethod.cfg.getAllBlocks.asScala.foldLeft(new HashMap[Block, Set[Block]])({
      (acc, block) => acc + (block -> new HashSet[Block])
    })

    graph.asScala.foreach({
      y =>
        val xs: Iterator[BrboNode] = dominanceFrontiers.getDominanceFrontier(y).asScala
        xs.foreach({
          x => map = map + (x.block -> (map(x.block) + y.block))
        })
    })

    // Remove the self control dependency
    map.foldLeft(new HashMap[Block, Set[Block]])({
      case (acc, (block, blocks)) => acc + (block -> (blocks - block))
    })
  }

  def reverseControlDependency(dependency: Map[Block, Set[Block]]): Map[Block, Set[Block]] = {
    var map = dependency.keySet.foldLeft(new HashMap[Block, Set[Block]])({
      (acc, block) => acc + (block -> new HashSet[Block])
    })

    dependency.foreach({
      case (block, blocks) =>
        blocks.foreach({
          block2 => map = map + (block2 -> (map(block2) + block))
        })
    })

    map
  }
}
