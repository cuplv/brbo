package brbo.numabschecker

import org.checkerframework.common.basetype.{BaseAnnotatedTypeFactory, BaseTypeChecker}

class NumabsAnnotatedTypeFactory(checker: BaseTypeChecker) extends BaseAnnotatedTypeFactory(checker) {
  this.postInit()
}
