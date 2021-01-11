package brbo.boundinference

import brbo.common.InstrumentUtils.FileFormat.JAVA_FORMAT
import brbo.common.InstrumentUtils.InstrumentMode.ALL
import brbo.common.TypeUtils.BrboType.INT
import brbo.common.{InstrumentUtils, TypeUtils}
import javax.annotation.processing.SupportedAnnotationTypes
import javax.lang.model.`type`.TypeMirror
import org.apache.logging.log4j.LogManager
import org.checkerframework.javacutil.TreeUtils

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

@SupportedAnnotationTypes(Array("*"))
@deprecated
class BoundInferenceProcessor extends BasicProcessor {
  private val logger = LogManager.getLogger(classOf[BoundInferenceProcessor])

  override def runAnalysis(): Unit = {
  }

  /*def generateSourceCodeDeltaUpdates(): String = {
    getMethods.head match {
      case (methodTree, cfg) =>
        // Default decomposition
        // TODO: What if R is reset by more than once?
        val result =
          InstrumentUtils.substituteAtomicStatements(
            methodTree.getBody,
            InstrumentUtils.defaultResourceAssignment,
            indent,
            cfg,
            getLineNumber,
            ALL
          )
        // TODO: A very hacky way to insert the declaration at the entry
        val newMethodBody = {
          val spaces = " " * indent
          result.result.replaceFirst("\\{", s"{\n$spaces${spaces}int ${InstrumentUtils.defaultDeltaVariable} = 0;")
        }
        InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(methodTree, getEnclosingClass(methodTree).get.getSimpleName.toString, newMethodBody, JAVA_FORMAT, indent)
    }
  }

  def generateSourceCodeNoResourceUpdates(): String = {
    getMethods.head match {
      case (methodTree, cfg) =>
        val result =
          InstrumentUtils.substituteAtomicStatements(
            methodTree.getBody,
            InstrumentUtils.removeResourceAssignment,
            indent,
            cfg,
            getLineNumber,
            ALL
          )
        InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(methodTree, getEnclosingClass(methodTree).get.getSimpleName.toString, result.result, JAVA_FORMAT, indent)
    }
  }*/
}
