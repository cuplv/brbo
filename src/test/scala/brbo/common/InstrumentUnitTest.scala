package brbo.common

import brbo.TestFiles
import brbo.boundinference.BasicProcessor
import brbo.common.Instrument.AtomicStatementInstrumentation
import brbo.common.Instrument.GhostVariable.{Counter, Delta, Resource}
import brbo.common.Instrument.InstrumentMode.{ALL, AT_MOST_ONCE}
import org.scalatest.flatspec.AnyFlatSpec

class InstrumentUnitTest extends AnyFlatSpec {
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

  "Instrumentation" should "output correct java source code without any instrumentation" in {
    TestFiles.unitTests.foreach({
      testCase =>
        val basicProcessor = new BasicProcessor
        JavacUtils.runProcessor(testCase.name, testCase.sourceCode, basicProcessor)
        val results = basicProcessor.testInstrumentation(AtomicStatementInstrumentation(_ => false, tree => tree.toString), AT_MOST_ONCE)
        assert(results.size == 1, "We should have only 1 method per test class")
        results.foreach({
          case (_, result) => assert(result.result == testCase.expectedOutput, s"${result.result}")
        })
    })
  }

  it should s"output correct java source code when replacing `R = R + e` with `d = d + e` in mode `$AT_MOST_ONCE`" in {
    TestFiles.replaceResourceAssignmentsAtMostOnce.foreach({
      testCase =>
        val basicProcessor = new BasicProcessor
        JavacUtils.runProcessor(testCase.name, testCase.sourceCode, basicProcessor)
        val results = basicProcessor.testInstrumentation(Instrument.defaultResourceAssignment, AT_MOST_ONCE)

        assert(results.size == 1, "We should have exactly 1 method per test class")
        results.foreach({
          case (_, result) =>
            // println(result.result)
            // println(testCase.expectedOutput)
            assert(result.result == testCase.expectedOutput, s"${result.result}")
        })
    })
  }

  it should s"output correct java source code when replacing `R = R + e` with `d = d + e` in mode `$ALL`" in {
    TestFiles.replaceResourceAssignmentsAll.foreach({
      testCase =>
        val basicProcessor = new BasicProcessor
        JavacUtils.runProcessor(testCase.name, testCase.sourceCode, basicProcessor)
        val results = basicProcessor.testInstrumentation(Instrument.defaultResourceAssignment, ALL)

        assert(results.size == 1, "We should have exactly 1 method per test class")
        results.foreach({
          case (_, result) =>
            // println(result.result)
            // println(testCase.expectedOutput)
            assert(result.result == testCase.expectedOutput, s"${result.result}")
        })
    })
  }
}
