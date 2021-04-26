package brbo.verification.decomposition

/**
 *
 * @param allVariables Local and input variables that taint resource updates
 * @param inputs       Input variables that taint resource updates
 */
case class TaintSet(allVariables: Set[String], inputs: Set[String])
