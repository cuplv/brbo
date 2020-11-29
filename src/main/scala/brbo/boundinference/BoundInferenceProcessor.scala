package brbo.boundinference

import brbo.common.Instrument
import brbo.common.Instrument.GhostVariable.Resource
import brbo.common.Instrument.{AtomicStatementInstrumentation, InstrumentResult}
import com.sun.source.tree.{MethodTree, Tree}
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

        val deltaVariable = "D100"

        val instrumentedSourceCode =
          Instrument.substituteAtMostOneAtomicStatement(
            methodTree.getBody,
            AtomicStatementInstrumentation( // Replace R = R + e with d = d + e
              {
                node: Node => Instrument.extractGhostVariableFromAssignment(node, Resource).nonEmpty
              },
              {
                tree: Tree =>
                  /*tree match {
                    case expressionStatement: ExpressionStatementTree =>
                      expressionStatement.getExpression match {
                        case assignmentTree: AssignmentTree =>
                          val update = Instrument.extractGhostVariableFromAssignment(assignmentTree, Resource)
                          s"$deltaVariable = $deltaVariable + ${update.get.update}"
                        case unaryTree: UnaryTree =>
                          unaryTree.getKind match {
                            case Tree.Kind.PREFIX_DECREMENT | Tree.Kind.POSTFIX_DECREMENT => s"$deltaVariable = $deltaVariable - 1"
                            case Tree.Kind.PREFIX_INCREMENT | Tree.Kind.POSTFIX_INCREMENT => s"$deltaVariable = $deltaVariable + 1"
                          }
                        case _ => throw new RuntimeException(s"Instrumenting non assignment statement `$tree` (Kind: ${tree.getKind})")
                      }
                    case _ => throw new RuntimeException(s"Instrumenting non expression statement `$tree` (Kind: ${tree.getKind})")
                  }*/
                  tree.toString
              }),
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
