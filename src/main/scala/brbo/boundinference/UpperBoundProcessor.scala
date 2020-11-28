package brbo.boundinference

import javax.annotation.processing.SupportedAnnotationTypes
import org.apache.logging.log4j.LogManager

@SupportedAnnotationTypes(Array("*"))
class UpperBoundProcessor extends AbstractProcessor {
  private val logger = LogManager.getLogger(classOf[UpperBoundProcessor])

  private def inferVariableUpperBound(ghostVariable: String, sourceCode: String,
                                      instrumentedSourceCode: String, inputVariables: Set[String]): String = {
    ???
  }

  override def runAnalysis(): Unit = ???
}
