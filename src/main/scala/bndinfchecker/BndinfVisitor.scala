package bndinfchecker

import com.sun.source.tree.{ClassTree, MethodTree}
import org.apache.logging.log4j.LogManager
import org.checkerframework.common.basetype.{BaseAnnotatedTypeFactory, BaseTypeChecker, BaseTypeVisitor}
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.UnderlyingAST.CFGMethod
import org.checkerframework.dataflow.cfg.builder.CFGBuilder
import org.checkerframework.javacutil.TreeUtils

// import scala.collection.JavaConverters._

class BndinfVisitor(checker: BaseTypeChecker) extends BaseTypeVisitor[BaseAnnotatedTypeFactory](checker) {
  private val logger = LogManager.getLogger(classOf[BndinfVisitor])

  override def visitMethod(node: MethodTree, p: Void): Void = {
    logger.debug(s"Visiting method: ${node.getName}")
    if (node.getBody == null)
      return null

    val underlyingAST = new CFGMethod(node, getEnclosingClass(node))
    val cfg: ControlFlowGraph = CFGBuilder.build(root, underlyingAST, false, true, this.checker.getProcessingEnvironment)

    PrintCFG.print(cfg)

    null
    // super.visitMethod(node, p)
  }

  private def getEnclosingClass(node: MethodTree): ClassTree = TreeUtils.enclosingClass(atypeFactory.getPath(node))
}
