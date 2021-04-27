package brbo.verification.decomposition

import brbo.common.GhostVariableUtils
import brbo.common.GhostVariableUtils.GhostVariable.Resource

/**
 *
 * @param allVariables Local and input variables that taint resource updates
 * @param inputs       Input variables that taint resource updates
 */
case class TaintSet(allVariables: Set[String], inputs: Set[String]) {
  def toTestString: String = s"All: ${allVariables.toList.sorted}. Inputs: ${inputs.toList.sorted}"
}

object TaintSet {
  def merge(taintSets: Iterable[TaintSet]): TaintSet = {
    TaintSet(taintSets.flatMap(set => set.allVariables).toSet, taintSets.flatMap(set => set.inputs).toSet)
  }

  def removeResourceVariables(taintSet: TaintSet): TaintSet = {
    TaintSet(
      taintSet.allVariables.filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource)),
      taintSet.inputs.filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource))
    )
  }
}