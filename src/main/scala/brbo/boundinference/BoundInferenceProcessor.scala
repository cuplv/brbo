package brbo.boundinference

import brbo.common.JavacUtils
import javax.annotation.processing.SupportedAnnotationTypes
import org.apache.logging.log4j.LogManager


@SupportedAnnotationTypes(Array("*"))
class BoundInferenceProcessor(compilationUnitName: String, sourceFileContents: String) extends AbstractProcessor {
  private val logger = LogManager.getLogger(classOf[BoundInferenceProcessor])

  override def runAnalysis(): Unit = {
    val upperBoundProcessor = new UpperBoundProcessor
    // JavacUtils.runProcessor(compilationUnitName, ???, upperBoundProcessor)
  }
}
