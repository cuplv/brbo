package numabschecker

import com.sun.source.tree.MethodTree
import org.checkerframework.common.basetype.{BaseAnnotatedTypeFactory, BaseTypeChecker, BaseTypeVisitor}

class NumabsVisitor(checker: BaseTypeChecker) extends BaseTypeVisitor[BaseAnnotatedTypeFactory](checker) {
  override def visitMethod(node: MethodTree, p: Void): Void = {
    logger.trace(s"Visiting method: ${node.getName}")
    super.visitMethod(node, p)
  }
}
