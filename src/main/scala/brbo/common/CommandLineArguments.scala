package brbo.common

import brbo.verification.AmortizationMode.{AmortizationMode, UNKNOWN}

case class CommandLineArguments(amortizationMode: AmortizationMode,
                                debugMode: Boolean,
                                directoryToAnalyze: String,
                                skipSanityCheck: Boolean,
                                printCounterExample: Boolean,
                                printIcraInputs: Boolean,
                                icraTimeout: Int)

object CommandLineArguments {
  val DEFAULT_ICRA_TIME_OUT = 15 // Unit: Second

  val DEFAULT_ARGUMENTS: CommandLineArguments = CommandLineArguments(UNKNOWN, debugMode = false, "", skipSanityCheck = false, printCounterExample = false, printIcraInputs = false, icraTimeout = 20)
}