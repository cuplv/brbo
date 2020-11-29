package brbo.boundinference

import brbo.common.Instrument
import javax.annotation.processing.SupportedAnnotationTypes
import org.apache.logging.log4j.LogManager

import scala.collection.JavaConverters._

/**
 *
 * @param compilationUnitName
 * @param sourceFileContents The source code from which this processor runs on
 */
@SupportedAnnotationTypes(Array("*"))
class BoundInferenceProcessor(compilationUnitName: String, sourceFileContents: String) extends AbstractProcessor {
  private val logger = LogManager.getLogger(classOf[BoundInferenceProcessor])

  override def runAnalysis(): Unit = {
    getMethods.foreach({
      case (methodTree, cfg) =>
        // Default decomposition
        val instrumentedSourceCode = {
          val result = Instrument.substituteAtMostOneAtomicStatement(
            methodTree.getBody,
            Instrument.defaultResourceAssignment,
            0,
            cfg,
            getLineNumber
          )
          // TODO: A very hacky way to insert the declaration at the entry
          val spaces = " " * Instrument.INDENT
          result.result.replaceFirst("\\{", s"{\n${spaces}int ${Instrument.defaultDeltaVariable} = 0;")
        }

        println(instrumentedSourceCode)

        /*val noResourceVariableSourceCode =
          Instrument.substituteAtMostOneAtomicStatement( // TODO: Should be substitute all
            methodTree.getBody,
            Instrument.removeResourceAssignment,
            0,
            cfg,
            getLineNumber
          ).result

        println(noResourceVariableSourceCode)*/

        cfg.getAllNodes.asScala

      // val upperBoundProcessor = new UpperBoundProcessor(sourceFileContents, "D", ???)
      // JavacUtils.runProcessor(compilationUnitName, instrumentedSourceCode.result, upperBoundProcessor)
    })
  }
}
