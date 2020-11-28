package brbo.boundinference

import brbo.TestFiles
import brbo.common.JavacUtils
import org.scalatest.flatspec.AnyFlatSpec

class BoundInferenceProcessorTest extends AnyFlatSpec {
  "BoundInferenceProcessor" should "not throw exception" in {
    TestFiles.testFiles.foreach({
      case (sourceFileName, sourceCode) =>
        val boundInferenceProcessor = new BoundInferenceProcessor(sourceFileName + ".java", sourceCode)
        JavacUtils.runProcessor(sourceFileName, sourceCode, boundInferenceProcessor)
    })
  }
}