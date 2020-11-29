package brbo.boundinference

import brbo.TestFiles
import brbo.common.Instrument.AtomicStatementInstrumentation
import brbo.common.{Instrument, JavacUtils}
import org.scalatest.flatspec.AnyFlatSpec

class BoundInferenceProcessorUnitTest extends AnyFlatSpec {
  "BoundInferenceProcessor" should "output correct java source code without any instrumentation" in {
    TestFiles.unitTests.foreach({
      testCase =>
        val boundInferenceProcessor = new BoundInferenceProcessor(testCase.name + ".java", testCase.sourceCode)
        JavacUtils.runProcessor(testCase.name, testCase.sourceCode, boundInferenceProcessor)
        val results = boundInferenceProcessor.testInstrumentation(AtomicStatementInstrumentation(_ => false, tree => tree.toString))
        assert(results.size == 1, "We should have only 1 method per test class")
        results.foreach({
          case (_, result) => assert(result.result == testCase.expectedOutput, s"${result.result}")
        })
    })
  }

  it should "output correct java source code when replacing `R = R + e` with `d = d + e`" in {
    TestFiles.replaceResourceAssignments.foreach({
      testCase =>
        val boundInferenceProcessor = new BoundInferenceProcessor(testCase.name + ".java", testCase.sourceCode)
        JavacUtils.runProcessor(testCase.name, testCase.sourceCode, boundInferenceProcessor)

        val results = boundInferenceProcessor.testInstrumentation(Instrument.defaultResourceAssignment)

        assert(results.size == 1, "We should have only 1 method per test class")
        results.foreach({
          case (_, result) =>
            assert(result.result == testCase.expectedOutput, s"${result.result}")
        })
    })
  }
}