package brbo.boundinference

import javax.annotation.processing.SupportedAnnotationTypes
import org.apache.logging.log4j.LogManager

/**
 * @param sourceCodeNoResourceUpdates The source code without any updates to resource variables
 * @param ghostVariable               The ghost variable that we wish to upper-bound
 * @param boundVocabulary             The vocabulary used in the upper bound
 */
@SupportedAnnotationTypes(Array("*"))
class UpperBoundProcessor(sourceCodeNoResourceUpdates: String, ghostVariable: String, boundVocabulary: Set[String]) extends BasicProcessor {
  private val logger = LogManager.getLogger(classOf[UpperBoundProcessor])

  private def inferVariableUpperBound(instrumentedSourceCode: String): String = {
    ???
  }

  override def runAnalysis(): Unit = {
    assumeOneClassOneMethod()

    // For each d = d + e, generate a file with d = 0 inserted and another file with c = 0 and c = c + 1 inserted
  }
}
