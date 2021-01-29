package brbo.verification

object AmortizationMode extends Enumeration {
  type AmortizationMode = Value
  val NO_AMORTIZE, FULL_AMORTIZE, SELECTIVE_AMORTIZE, ALL_AMORTIZE, UNKNOWN = Value

  def amortizationModeToShortString(amortizationMode: AmortizationMode): String = {
    amortizationMode match {
      case NO_AMORTIZE => "noAmortize"
      case FULL_AMORTIZE => "fullAmortize"
      case SELECTIVE_AMORTIZE => "selectiveAmortize"
      case ALL_AMORTIZE => "allAmortize"
      case UNKNOWN => "unknown"
    }
  }
}