package brbo

import brbo.common.{CommandLineArguments, CommandLineArgumentsReflect, TargetMethod, Z3Solver}
import brbo.verification.AmortizationMode.ALL_AMORTIZE
import brbo.verification.BoundChecking.GlobalInvariants
import brbo.verification.Decomposition.DecompositionResult
import brbo.verification.{BasicProcessor, BoundChecking, Decomposition}
import org.apache.commons.io.{FileUtils, FilenameUtils}
import org.apache.logging.log4j.LogManager

import java.io.File
import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

object BrboMain {
  private val logger = LogManager.getLogger("brbo.BrboMain")

  def main(args: Array[String]) {
    logger.info("Brbo has started.")

    val commandLineArguments = CommandLineArgumentsReflect.parseArguments(args)
    logger.info(s"Command line arguments:\n$commandLineArguments")
    logger.warn(s"We assume each class contains exactly one method")

    val sourceFiles: Map[File, String] = {
      val file = new java.io.File(commandLineArguments.directoryToAnalyze)
      val allFiles: Array[File] = {
        if (file.isDirectory) FileUtils.listFiles(file, Array("java"), true).asScala.toArray
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
      case (sourceFile: File, sourceFileContents: String) =>
        checkBound(sourceFile.getAbsolutePath, sourceFileContents, commandLineArguments)
    })
  }

  /**
   *
   * @param sourceFilePath Used to extract class name
   * @param sourceFileContents
   * @param commandLineArguments
   * @return
   */
  def decompose(sourceFilePath: String, sourceFileContents: String, commandLineArguments: CommandLineArguments): Option[List[DecompositionResult]] = {
    logger.info(s"Infer invariants for the resource variable in file `$sourceFilePath`")

    val className: String = {
      val prefix = """src/main/java/"""
      val indexOfPrefix = sourceFilePath.indexOf(prefix)
      val almostClassName = {
        if (indexOfPrefix != -1) {
          sourceFilePath.substring(indexOfPrefix + prefix.length)
        }
        else sourceFilePath
      }
      val indexOfExtension = FilenameUtils.indexOfExtension(almostClassName)
      almostClassName.replace("""/""", ".").substring(0, indexOfExtension)
    }
    logger.info(s"Class name: `$className`")

    if (className != "brbo.benchmarks.Common") {
      logger.info(s"Parsing...")
      val targetMethod: TargetMethod = BasicProcessor.getTargetMethod(className, sourceFileContents)
      val decomposition: Decomposition = new Decomposition(targetMethod, commandLineArguments)
      Some(decomposition.decompose(commandLineArguments))
    }
    else {
      logger.info(s"Skipping bound checking for file `$sourceFilePath`")
      None
    }
  }

  /**
   *
   * @param sourceFilePath Used to extract class name
   * @param sourceFileContents
   * @param commandLineArguments
   */
  def checkBound(sourceFilePath: String, sourceFileContents: String, commandLineArguments: CommandLineArguments): Unit = {
    decompose(sourceFilePath, sourceFileContents, commandLineArguments) match {
      case Some(decompositionResults) =>
        decompositionResults.foreach({ result => BoundChecking.extractBoundAndCheck(result, commandLineArguments) })
      case None =>
    }
  }

  /**
   *
   * @param solver
   * @param sourceFilePath Used to extract class name
   * @param sourceFileContents
   * @param commandLineArguments
   * @return
   */
  def inferResourceInvariants(solver: Z3Solver, sourceFilePath: String, sourceFileContents: String, commandLineArguments: CommandLineArguments): Option[GlobalInvariants] = {
    assert(commandLineArguments.amortizationMode != ALL_AMORTIZE, "Expect choosing one amortization mode")

    decompose(sourceFilePath, sourceFileContents, commandLineArguments) match {
      case Some(decompositionResults) =>
        val globalInvariants = decompositionResults.map({ result => BoundChecking.inferInvariantsForResource(solver, result, commandLineArguments) })
        assert(globalInvariants.size == 1, "Expect that choosing one amortization mode leads to one decomposition result")
        Some(globalInvariants.head)
      case None => None
    }
  }

  private def readFromFile(location: String): String = {
    val source = scala.io.Source.fromFile(location)
    try source.mkString finally source.close()
  }
}
