package brbo.verification

object AmortizationMode extends Enumeration {
  type AmortizationMode = Value
  val NO_AMORTIZE, FULL_AMORTIZE, SELECTIVE_AMORTIZE, ALL_AMORTIZE, UNKNOWN = Value
}