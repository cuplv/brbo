package brbo.boundinference

import brbo.common.Instrument
import brbo.common.Instrument.AtomicStatementInstrumentation
import brbo.common.Instrument.GhostVariable.Resource
import com.sun.source.tree.{AssignmentTree, ExpressionStatementTree, Tree, UnaryTree}
import javax.annotation.processing.SupportedAnnotationTypes
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.node.Node

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
        // Insert d = 0 at the entry
        cfg.getAllNodes.asScala

        val instrumentedSourceCode =
          Instrument.substituteAtMostOneAtomicStatement(
            methodTree.getBody,
            Instrument.defaultResourceAssignment,
            0,
            cfg,
            getLineNumber
          )

        println(instrumentedSourceCode.result)

      // val upperBoundProcessor = new UpperBoundProcessor(sourceFileContents, "D", ???)
      // JavacUtils.runProcessor(compilationUnitName, instrumentedSourceCode.result, upperBoundProcessor)
    })
  }
}
