package brbo.verification.decomposition

import brbo.common.TargetMethod
import brbo.verification.AmortizationMode.AmortizationMode
import brbo.verification.BasicProcessor
import org.apache.logging.log4j.LogManager

/**
 *
 * @param newSourceFileContents The source code after decomposing the input method
 * @param deltaCounterPairs     The paris of delta variables and counters in the new source code
 * @param amortizationMode      The mode that was used to construct this decomposition
 * @param inputMethod           The input method that was decomposed
 */
case class DecompositionResult(newSourceFileContents: String,
                               deltaCounterPairs: Set[DeltaCounterPair],
                               amortizationMode: AmortizationMode,
                               inputMethod: TargetMethod) {
  DecompositionResult.logger.info(s"Decomposition result (Mode: `$amortizationMode`):\n$newSourceFileContents")
  val outputMethod: TargetMethod = BasicProcessor.getTargetMethod(inputMethod.fullQualifiedClassName, newSourceFileContents)
  assert(outputMethod.inputVariables == inputMethod.inputVariables)
}

object DecompositionResult {
  private val logger = LogManager.getLogger("brbo.verification.decomposition.DecompositionResult")
}