package brbo.verification

import brbo.common.TargetMethod
import brbo.common.TypeUtils.BrboType.BrboType
import com.sun.source.tree.Tree
import org.apache.logging.log4j.LogManager

class Decomposition(targetMethod: TargetMethod) {
  private val logger = LogManager.getLogger(classOf[Decomposition])

  def computeDependencySet(location: Tree): Set[String] = {
    ???
  }

  def computeModifiedSet(location: Tree): Set[String] = {
    ???
  }

  def extractSubprogram(location: Tree): String = {
    ???
  }
}
