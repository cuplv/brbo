package bndinfchecker

import org.checkerframework.common.basetype.{BaseAnnotatedTypeFactory, BaseTypeChecker, BaseTypeVisitor}

class BndinfVisitor(checker: BaseTypeChecker) extends BaseTypeVisitor[BaseAnnotatedTypeFactory](checker) {
  override def visitMethod(node: Nothing, p: Void): Void = {
    super.visitMethod(node, p)
  }
}
