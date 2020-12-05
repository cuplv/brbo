package brbo.boundinference

import brbo.boundinference.FileFormat.JAVA_FORMAT
import brbo.common.Instrument.InstrumentMode.ALL
import brbo.common.TypeUtils.BrboType.INT
import brbo.common.{Instrument, JavacUtils, TypeUtils}
import javax.annotation.processing.SupportedAnnotationTypes
import javax.lang.model.`type`.TypeMirror
import org.apache.logging.log4j.LogManager
import org.checkerframework.javacutil.TreeUtils

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

@SupportedAnnotationTypes(Array("*"))
class BoundInferenceProcessor extends BasicProcessor {
  private val logger = LogManager.getLogger(classOf[BoundInferenceProcessor])

  override def runAnalysis(): Unit = {
    assumeOneClassOneMethod()

    // Default decomposition
    val sourceCodeDeltaUpdates = generateSourceCodeDeltaUpdates()
    // println(sourceCodeDeltaUpdates)
    val sourceCodeNoResourceUpdates = generateSourceCodeNoResourceUpdates()
    // println(sourceCodeNoResourceUpdates)

    val boundVocabulary = generateBoundVocabulary()
    logger.info(s"Inferring bounds with vocabulary `$boundVocabulary`")

    val upperBoundProcessor = new UpperBoundProcessor(sourceCodeNoResourceUpdates, Instrument.defaultDeltaVariable, boundVocabulary)
    JavacUtils.runProcessor(getCompilationUnitName, sourceCodeDeltaUpdates, upperBoundProcessor)
    upperBoundProcessor.runAnalysis()
  }

  def generateSourceCodeDeltaUpdates(): String = {
    getMethods.head match {
      case (methodTree, cfg) =>
        // Default decomposition
        // TODO: What if R is reset by more than once?
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
        replaceMethodBodyAndGenerateSourceCode(methodTree, getEnclosingClass(methodTree).get.getSimpleName.toString, newMethodBody, JAVA_FORMAT)
    }
  }

  def generateSourceCodeNoResourceUpdates(): String = {
    getMethods.head match {
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
        replaceMethodBodyAndGenerateSourceCode(methodTree, getEnclosingClass(methodTree).get.getSimpleName.toString, result.result, JAVA_FORMAT)
    }
  }

  def generateBoundVocabulary(): HashSet[String] = {
    getMethods.head match {
      case (methodTree, _) =>
        val parameters = methodTree.getParameters.asScala.foldLeft(HashMap[String, TypeMirror]())({
          (acc, param) => acc + (param.getName.toString -> TreeUtils.typeOf(param.getType))
        })
        TypeUtils.typeMapTranslation(parameters).foldLeft(new HashSet[String])({
          case (acc, (name, typ)) => if (typ == INT) acc + name else acc
        })
    }
  }
}
