package brbo.common

import brbo.verification.AmortizationMode.{AmortizationMode, UNKNOWN}

case class CommandLineArguments(amortizationMode: AmortizationMode,
                                debugMode: Boolean,
                                directoryToAnalyze: String,
                                skipSanityCheck: Boolean,
                                printCounterExample: Boolean,
                                printIcraInputs: Boolean,
                                icraTimeout: Int) {
  override def toString: String = {
    val directoryToAnalyzeString = s"Infer resource usage upper bounds for each method in each file `*.java` under directory `$directoryToAnalyze`"
    val amortizationModeString = s"Amortization mode: `$amortizationMode`"
    val debugModeString = s"Debug mode? `$debugMode`"
    val skipSanityCheckString = s"Skip sanity check? `$skipSanityCheck`"
    val printCounterExampleString = s"Print counter examples if cannot verify the bound? `$printCounterExample`"
    val printIcraInputsString = s"Print inputs to ICRA? `$printIcraInputs`"
    val icraTimeoutString = s"ICRA's time out: `$icraTimeout` seconds"
    s"$directoryToAnalyzeString\n$amortizationModeString\n$debugModeString\n$skipSanityCheckString\n$printCounterExampleString\n$printIcraInputsString\n$icraTimeoutString"
  }
}

object CommandLineArguments {
  val DEFAULT_ICRA_TIME_OUT = 15 // Unit: Second

  val DEFAULT_ARGUMENTS: CommandLineArguments = CommandLineArguments(UNKNOWN, debugMode = false, "", skipSanityCheck = false, printCounterExample = false, printIcraInputs = false, icraTimeout = 20)
}