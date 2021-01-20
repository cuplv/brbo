package brbo

import java.io.File

import brbo.common.{CommandLineArguments, TargetMethod, Z3Solver}
import brbo.verification.Decomposition.DecompositionResult
import brbo.verification.{BasicProcessor, BoundChecking, Decomposition}
import com.microsoft.z3.AST
import org.apache.commons.io.FilenameUtils
import org.apache.logging.log4j.LogManager

import scala.collection.immutable.HashMap

object BrboMain {
  private val logger = LogManager.getLogger("brbo.BrboMain")

  private def readFromFile(location: String): String = {
    val source = scala.io.Source.fromFile(location)
    try source.mkString finally source.close()
  }

  def main(args: Array[String]) {
    logger.info("Brbo has started: Infer resource usage upper bounds for each method.")

    val commandLineArguments = CommandLineArguments.parseArguments(args)
    logger.info(s"Analyze files under directory `${commandLineArguments.getDirectoryToAnalyze}`")
    logger.info(s"Amortization mode: `${commandLineArguments.getAmortizationMode}`")
    logger.info(s"Debug mode? `${commandLineArguments.getDebugMode}`")

    val sourceFiles: Map[File, String] = {
      val file = new java.io.File(commandLineArguments.getDirectoryToAnalyze)
      val allFiles: Array[File] = {
        if (file.isDirectory) file.listFiles
        else Array(file)
      }
      val allJavaFilePaths = allFiles.filter(_.getName.endsWith(".java"))
      allJavaFilePaths.foldLeft(new HashMap[File, String])({
        (acc, sourceFileLocation) =>
          logger.info(s"Reading from source file `$sourceFileLocation`")
          acc + (sourceFileLocation -> readFromFile(sourceFileLocation.getAbsolutePath))
      })
    }

    sourceFiles.foreach({
      case (sourceFile, sourceFileContents) =>
        val sourceFilePath = sourceFile.getAbsolutePath
        logger.info(s"Inferring bound for file `$sourceFilePath`")

        val className: String = {
          val prefix = """src/main/java/"""
          val almostClassName = sourceFilePath.substring(sourceFilePath.indexOf(prefix) + prefix.length)
          val indexOfExtension = FilenameUtils.indexOfExtension(almostClassName)
          almostClassName.replace("""/""", ".").substring(0, indexOfExtension)
        }
        logger.info(s"Class name: `$className`")

        if (className != "brbo.benchmarks.Common") {
          logger.info(s"Parsing...")
          val targetMethod: TargetMethod = BasicProcessor.getTargetMethod(className, sourceFileContents)

          val decomposition: Decomposition = new Decomposition(targetMethod, commandLineArguments.getDebugMode)
          val decompositionResult: DecompositionResult = {
            logger.info(s"Decomposing...")
            val subprograms = decomposition.decomposeSelectiveAmortize()
            logger.info(s"Inserting resets and updates to ghost variables...")
            decomposition.insertGhostVariables(subprograms)
          }

          val solver: Z3Solver = new Z3Solver
          val boundExpression: AST = BoundChecking.extractBoundExpression(solver, targetMethod.methodTree, targetMethod.inputVariables ++ targetMethod.localVariables)
          logger.info(s"Extracted bound expression is `$boundExpression`")
          logger.info(s"Checking bound...")
          BoundChecking.checkBound(solver, decompositionResult, boundExpression, printModelIfFail = true)
        }
        else {
          logger.info(s"Skipping bound checking for file `$sourceFilePath`")
        }
    })
  }
}
