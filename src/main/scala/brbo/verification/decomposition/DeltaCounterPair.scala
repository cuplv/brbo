package brbo.verification.decomposition

import brbo.common.GhostVariableUtils
import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta}

case class DeltaCounterPair(delta: String, counter: String) {
  GhostVariableUtils.isGhostVariable(delta, Delta)
  GhostVariableUtils.isGhostVariable(counter, Counter)
}