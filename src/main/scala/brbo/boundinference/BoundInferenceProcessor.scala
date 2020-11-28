package brbo.boundinference

import brbo.common.Instrument
import brbo.common.Instrument.AtomicStatementInstrumentation
import brbo.common.Instrument.GhostVariable.Resource
import com.sun.source.tree.{AssignmentTree, Tree}
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
        // Replace all R = R + e with d = d + e
        cfg.getAllNodes.asScala

        val deltaVariable = "D100"

        val instrumentedSourceCode =
          Instrument.substituteAtMostOneAtomicStatement(
            methodTree.getBody,
            AtomicStatementInstrumentation(
              {
                node: Node => Instrument.extractGhostVariableFromAssignment(node, Resource).nonEmpty
              },
              {
                tree: Tree =>
                  tree match {
                    case assignmentTree: AssignmentTree =>
                      val update = Instrument.extractGhostVariableFromAssignment(assignmentTree, Resource)
                      s"$deltaVariable = $deltaVariable + ${update.get.update}"
                    case _ => throw new RuntimeException(s"Instrumenting non-assignment statement `$tree`")
                  }
              }),
            0,
            cfg,
            getLineNumber
          )

        println(instrumentedSourceCode)

      // val upperBoundProcessor = new UpperBoundProcessor(sourceFileContents, "D", ???)
      // JavacUtils.runProcessor(compilationUnitName, instrumentedSourceCode.result, upperBoundProcessor)
    })
  }
}
