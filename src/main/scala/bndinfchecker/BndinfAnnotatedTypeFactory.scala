package bndinfchecker

import org.checkerframework.common.basetype.{BaseAnnotatedTypeFactory, BaseTypeChecker}

class BndinfAnnotatedTypeFactory (checker: BaseTypeChecker) extends BaseAnnotatedTypeFactory(checker) {
  this.postInit()
}
