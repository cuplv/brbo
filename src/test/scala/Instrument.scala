import common.Instrument
import org.scalatest.flatspec.AnyFlatSpec

class Instrument extends AnyFlatSpec {
  "Delta variable" should "start with D" in {
    assert(Instrument.isDeltaVariable("D"))
    assert(Instrument.isDeltaVariable("D123"))
    assert(!Instrument.isDeltaVariable("R"))
    assert(!Instrument.isDeltaVariable("R123"))
  }

  "Instrumenting a basic block" should "be correct" in {

  }
}
