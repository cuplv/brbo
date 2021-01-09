package brbo.common

import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta, Resource}
import org.scalatest.flatspec.AnyFlatSpec

class GhostVariableUtilsUnitTest extends AnyFlatSpec {
  "Ghost variable" should "be correctly identified" in {
    assert(GhostVariableUtils.isGhostVariable("D", Delta))
    assert(GhostVariableUtils.isGhostVariable("D123", Delta))
    assert(!GhostVariableUtils.isGhostVariable("D123", Resource))
    assert(!GhostVariableUtils.isGhostVariable("D123", Counter))

    assert(GhostVariableUtils.isGhostVariable("R", Resource))
    assert(GhostVariableUtils.isGhostVariable("R123", Resource))
    assert(!GhostVariableUtils.isGhostVariable("R123", Delta))
    assert(!GhostVariableUtils.isGhostVariable("R123", Counter))

    assert(GhostVariableUtils.isGhostVariable("C", Counter))
    assert(GhostVariableUtils.isGhostVariable("C123", Counter))
    assert(!GhostVariableUtils.isGhostVariable("C123", Resource))
    assert(!GhostVariableUtils.isGhostVariable("C123", Delta))
  }
}
