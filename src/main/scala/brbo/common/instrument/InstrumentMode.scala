package brbo.common.instrument

@deprecated
object InstrumentMode extends Enumeration {
  type InstrumentMode = Value
  val AT_MOST_ONCE, ALL = Value
}
