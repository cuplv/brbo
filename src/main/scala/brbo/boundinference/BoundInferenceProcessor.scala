package brbo.boundinference

import brbo.common.Instrument
import brbo.common.Instrument.InstrumentMode.{AT_MOST_ONCE, ALL}
import javax.annotation.processing.SupportedAnnotationTypes
import org.apache.logging.log4j.LogManager

import scala.collection.JavaConverters._

@SupportedAnnotationTypes(Array("*"))
class BoundInferenceProcessor extends BasicProcessor {
  private val logger = LogManager.getLogger(classOf[BoundInferenceProcessor])

  override def runAnalysis(): Unit = {
    getMethods.foreach({
      case (methodTree, cfg) =>
        // Default decomposition
        val instrumentedSourceCode = {
          val result = Instrument.substituteAtomicStatements(
            methodTree.getBody,
            Instrument.defaultResourceAssignment,
            0,
            cfg,
            getLineNumber,
            ALL
          )
          // TODO: A very hacky way to insert the declaration at the entry
          val spaces = " " * Instrument.INDENT
          result.result.replaceFirst("\\{", s"{\n${spaces}int ${Instrument.defaultDeltaVariable} = 0;")
        }

        println(instrumentedSourceCode)

        val noResourceVariableSourceCode =
          Instrument.substituteAtomicStatements(
            methodTree.getBody,
            Instrument.removeResourceAssignment,
            0,
            cfg,
            getLineNumber,
            ALL
          ).result

        println(noResourceVariableSourceCode)

        cfg.getAllNodes.asScala

      // val upperBoundProcessor = new UpperBoundProcessor(sourceFileContents, "D", ???)
      // JavacUtils.runProcessor(compilationUnitName, instrumentedSourceCode.result, upperBoundProcessor)
    })
  }
}
