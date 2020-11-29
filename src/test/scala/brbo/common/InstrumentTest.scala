package brbo.common

import brbo.common.Instrument.GhostVariable.{Counter, Delta, Resource}
import org.scalatest.flatspec.AnyFlatSpec

class InstrumentTest extends AnyFlatSpec {
  "Ghost variable" should "be correctly identified" in {
    assert(Instrument.isGhostVariable("D", Delta))
    assert(Instrument.isGhostVariable("D123", Delta))
    assert(!Instrument.isGhostVariable("D123", Resource))
    assert(!Instrument.isGhostVariable("D123", Counter))

    assert(Instrument.isGhostVariable("R", Resource))
    assert(Instrument.isGhostVariable("R123", Resource))
    assert(!Instrument.isGhostVariable("R123", Delta))
    assert(!Instrument.isGhostVariable("R123", Counter))

    assert(Instrument.isGhostVariable("C", Counter))
    assert(Instrument.isGhostVariable("C123", Counter))
    assert(!Instrument.isGhostVariable("C123", Resource))
    assert(!Instrument.isGhostVariable("C123", Delta))
  }
}
