package brbo.boundinference

import brbo.common.TypeUtils.BrboType.BrboType
import com.sun.source.tree.{MethodTree, Tree}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph

object Decomposition {
  private val logger = LogManager.getLogger("brbo.boundinference.Decomposition")

  def computeDependencySet(className: String,
                           methodTree: MethodTree,
                           getLineNumber: Tree => Int,
                           cfg: ControlFlowGraph,
                           location: Tree): Map[String, BrboType] = {
    ???
  }

  def computeModifiedSet(): Map[String, BrboType] = {
    ???
  }
}
