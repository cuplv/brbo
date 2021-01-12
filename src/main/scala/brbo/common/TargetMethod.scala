package brbo.common

import brbo.common.TypeUtils.BrboType.BrboType
import com.sun.source.tree.{MethodTree, Tree}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph

import scala.collection.immutable.HashMap

/**
 *
 * @param className     The class name of the method
 * @param methodTree    The method that we wish to analyze
 * @param getLineNumber A function to get line numbers
 * @param cfg           The control flow graph of the method
 */
case class TargetMethod(className: String,
                        methodTree: MethodTree,
                        getLineNumber: Tree => Int,
                        cfg: ControlFlowGraph) {
  private val logger = LogManager.getLogger(classOf[TargetMethod])

  val inputVariables: Map[String, BrboType] = TreeUtils.getAllInputVariables(methodTree)
  logger.debug(s"[Method ${methodTree.getName}] Input variables: $inputVariables")

  val localVariables: Map[String, BrboType] =
    if (methodTree.getBody == null) new HashMap[String, BrboType]
    else TreeUtils.getAllDeclaredVariables(methodTree.getBody)
  logger.debug(s"[Method ${methodTree.getName}] Local variables: $localVariables")
}
