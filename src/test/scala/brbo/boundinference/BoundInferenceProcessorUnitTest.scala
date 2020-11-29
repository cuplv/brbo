package brbo.boundinference

import brbo.TestFiles
import brbo.common.Instrument.AtomicStatementInstrumentation
import brbo.common.JavacUtils
import org.scalatest.flatspec.AnyFlatSpec

class BoundInferenceProcessorUnitTest extends AnyFlatSpec {
  "BoundInferenceProcessor" should "not throw exception" in {
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
}