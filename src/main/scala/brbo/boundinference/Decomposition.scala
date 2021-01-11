package brbo.boundinference

import brbo.common.TargetMethod
import brbo.common.TypeUtils.BrboType.BrboType
import com.sun.source.tree.Tree
import org.apache.logging.log4j.LogManager

class Decomposition(targetMethod: TargetMethod) {
  private val logger = LogManager.getLogger(classOf[Decomposition])

  def computeDependencySet(location: Tree): Map[String, BrboType] = {
    ???
  }

  def computeModifiedSet(): Map[String, BrboType] = {
    ???
  }
}
