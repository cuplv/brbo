package bndinfchecker

import com.sun.source.tree.MethodTree
import org.checkerframework.common.basetype.{BaseAnnotatedTypeFactory, BaseTypeChecker, BaseTypeVisitor}

class BndinfVisitor(checker: BaseTypeChecker) extends BaseTypeVisitor[BaseAnnotatedTypeFactory](checker) {
  override def visitMethod(node: MethodTree, p: Void): Void = {
    println(s"!!! ${node.getName}")
    super.visitMethod(node, p)
  }
}
