package brbo.verification.decomposition

import brbo.verification.AmortizationMode.AmortizationMode

case class IntermediateResult[T <: Segment](groups: Groups[T], amortizationMode: AmortizationMode)
