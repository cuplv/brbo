package brbo.boundinference

import brbo.common.Instrument.InstrumentMode.ALL
import brbo.common.{Instrument, JavacUtils}
import javax.annotation.processing.SupportedAnnotationTypes
import org.apache.logging.log4j.LogManager

import scala.collection.immutable.HashSet

@SupportedAnnotationTypes(Array("*"))
class BoundInferenceProcessor extends BasicProcessor {
  private val logger = LogManager.getLogger(classOf[BoundInferenceProcessor])

  private val indent = Instrument.INDENT

  override def runAnalysis(): Unit = {
    assumeOneClassOneMethod()

    // Default decomposition
    val sourceCodeDeltaUpdates = generateSourceCodeDeltaUpdates()
    println(sourceCodeDeltaUpdates)
    val sourceCodeNoResourceUpdates = generateSourceCodeNoResourceUpdates()
    println(sourceCodeNoResourceUpdates)

    val boundVocabulary = new HashSet[String]

    val upperBoundProcessor = new UpperBoundProcessor(sourceCodeNoResourceUpdates, Instrument.defaultDeltaVariable, boundVocabulary)
    JavacUtils.runProcessor(getCompilationUnitName, sourceCodeDeltaUpdates, upperBoundProcessor)
    upperBoundProcessor.runAnalysis()
  }

  def generateSourceCodeDeltaUpdates(): String = {
    val instrumented = getMethods.map({
      case (methodTree, cfg) =>
        // Default decomposition
        val result =
          Instrument.substituteAtomicStatements(
            methodTree.getBody,
            Instrument.defaultResourceAssignment,
            indent,
            cfg,
            getLineNumber,
            ALL
          )
        // TODO: A very hacky way to insert the declaration at the entry
        val newMethodBody = {
          val spaces = " " * indent
          result.result.replaceFirst("\\{", s"{\n$spaces${spaces}int ${Instrument.defaultDeltaVariable} = 0;")
        }
        replaceMethodBody(methodTree, getEnclosingClass(methodTree).get.getSimpleName.toString, newMethodBody)
    })
    assert(instrumented.size == 1)
    instrumented.head
  }

  def generateSourceCodeNoResourceUpdates(): String = {
    val instrumented = getMethods.map({
      case (methodTree, cfg) =>
        val result =
          Instrument.substituteAtomicStatements(
            methodTree.getBody,
            Instrument.removeResourceAssignment,
            indent,
            cfg,
            getLineNumber,
            ALL
          )
        replaceMethodBody(methodTree, getEnclosingClass(methodTree).get.getSimpleName.toString, result.result)
    })
    assert(instrumented.size == 1)
    instrumented.head
  }
}
