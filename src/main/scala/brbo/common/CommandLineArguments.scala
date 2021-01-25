package brbo.common

import brbo.verification.AmortizationMode.AmortizationMode

case class CommandLineArguments(amortizationMode: AmortizationMode,
                                debugMode: Boolean,
                                directoryToAnalyze: String,
                                skipSanityCheck: Boolean,
                                printCounterExample: Boolean,
                                printIcraInputs: Boolean)
