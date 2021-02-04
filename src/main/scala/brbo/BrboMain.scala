package brbo

import brbo.common.AnalysisResult.{RawResult, aggregateResultsIndividual, aggregateResultsSummary}
import brbo.common._
import brbo.verification.AmortizationMode.ALL_AMORTIZE
import brbo.verification.BoundChecking.GlobalInvariants
import brbo.verification.Decomposition.DecompositionResult
import brbo.verification.{BasicProcessor, BoundChecking, Decomposition}
import com.microsoft.z3.AST
import org.apache.commons.io.{FileUtils, FilenameUtils}
import org.apache.logging.log4j.LogManager

import java.io.File
import java.nio.charset.Charset
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

object BrboMain {
  private val logger = LogManager.getLogger("brbo.BrboMain")
  private val BATCH_SIZE = 200

  val OUTPUT_DIRECTORY: String = s"${System.getProperty("user.dir")}/output"

  def main(args: Array[String]) {
    logger.info("Brbo has started.")

    val arguments = CommandLineArguments.parseArguments(args)
    arguments.toString.split("\n").foreach(s => logger.info(s"Command line argument - $s"))
    logger.warn(s"We assume each class contains exactly one method")

    val sourceFiles: List[(File, String)] = {
      val file = new java.io.File(arguments.getDirectoryToAnalyze)
      val allFiles: Array[File] = {
        if (file.isDirectory) FileUtils.listFiles(file, Array("java"), true).asScala.toArray
        else Array(file)
      }
      val allJavaFilePaths: List[File] = allFiles.filter(_.getName.endsWith(".java")).toList.sortWith({
        case (f1, f2) => f1.getAbsolutePath < f2.getAbsolutePath
      })
      allJavaFilePaths.map({
        sourceFileLocation =>
          logger.info(s"Read from source file `$sourceFileLocation`")
          (sourceFileLocation, readFromFile(sourceFileLocation.getAbsolutePath))
      })
    }

    logger.info(s"Run Brbo on programs in batches. Batch size: `$BATCH_SIZE`")
    sourceFiles.grouped(BATCH_SIZE).zipWithIndex.foreach({
      case (batch, index) => runBatch(batch, index, sourceFiles.size, arguments)
    })
  }

  private def runBatch(sourceFiles: List[(File, String)], batchIndex: Int, totalFiles: Int, arguments: CommandLineArguments): Unit = {
    val batchString = s"${StringFormatUtils.integer(batchIndex * BATCH_SIZE, 3)}-${StringFormatUtils.integer((batchIndex + 1) * BATCH_SIZE - 1, 3)}"
    logger.info(s"Run `$batchIndex`-th batch`: $batchString")

    val directoryOrFile = FilenameUtils.getBaseName(arguments.getDirectoryToAnalyze)
    val date = new SimpleDateFormat("MMdd-HHmm").format(new Date) // YYYYMMdd-HHmm

    val results: List[List[RawResult]] = {
      sourceFiles.zipWithIndex.map({
        case ((sourceFile: File, sourceFileContents: String), index) =>
          val fileIndex = index + batchIndex * BATCH_SIZE
          val progress: Double = fileIndex.toDouble / totalFiles * 100
          logger.info(s"Verify `$fileIndex`-th input file. Progress: ${StringFormatUtils.float(progress, 2)}%")
          val decompositionResult = decompose(sourceFile.getAbsolutePath, sourceFileContents, arguments)
          checkBound(decompositionResult, arguments)
      })
    }

    logger.info(s"Write results to files. Aggregate results only under mode `$ALL_AMORTIZE`")
    val logDirectory = s"$date-${arguments.toFileName}"
    arguments.getAmortizationMode match {
      case ALL_AMORTIZE =>
        val aggregatedCsvFileContents = "programs,lines,verified,time,verified,time,verified,time\n" + aggregateResultsSummary(results).map(r => r.toCSV).mkString("\n")
        val aggregatedCsvFileContentsIndividual = {
          "program,lines,verified,time,verified,time,verified,time\n" + aggregateResultsIndividual(results).sortWith({
            case (r1, r2) => r1.files.head < r2.files.head
          }).map(r => r.toCSV).mkString("\n")
        }
        val aggregatedCsvFile = new File(s"$OUTPUT_DIRECTORY/$logDirectory/$directoryOrFile-summary-$date-${arguments.toFileName}-$batchString.csv")
        val aggregatedCsvIndividualFile = new File(s"$OUTPUT_DIRECTORY/$logDirectory/$directoryOrFile-individual-$date-${arguments.toFileName}-$batchString.csv")
        FileUtils.writeStringToFile(aggregatedCsvFile, aggregatedCsvFileContents, Charset.forName("UTF-8"))
        FileUtils.writeStringToFile(aggregatedCsvIndividualFile, aggregatedCsvFileContentsIndividual, Charset.forName("UTF-8"))
        logger.info(s"Write results to file `${aggregatedCsvFile.getAbsolutePath}`, ${aggregatedCsvIndividualFile.getAbsolutePath}")
      case _ =>
    }
    val rawCsvFileContents = "name,lines,time,verified,mode\n" + results.flatten.map(r => r.toCSV).mkString("\n")
    val rawCsvFile = new File(s"$OUTPUT_DIRECTORY/$logDirectory/$directoryOrFile-raw-$date-${arguments.toFileName}-$batchString.csv")
    logger.info(s"Write results to file `${rawCsvFile.getAbsolutePath}`")
    FileUtils.writeStringToFile(rawCsvFile, rawCsvFileContents, Charset.forName("UTF-8"))
  }

  /**
   *
   * @param sourceFilePath     Used to extract class name
   * @param sourceFileContents Source code
   * @param arguments          Command line arguments
   * @return
   */
  def decompose(sourceFilePath: String, sourceFileContents: String, arguments: CommandLineArguments): Option[List[DecompositionResult]] = {
    logger.info(s"Phase 1: Decompose")
    logger.info(s"Decompose file `$sourceFilePath`")

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
      if (arguments.getPrintCFG) {
        logger.info(s"Print CFG to `${CFGUtils.OUTPUT_DIRECTORY}`...")
        CFGUtils.printPDF(targetMethod.cfg, None)
        logger.info(s"${targetMethod.cfg.toString}")
      }
      val decomposition: Decomposition = new Decomposition(targetMethod, arguments)
      Some(decomposition.decompose(arguments))
    }
    else {
      logger.info(s"Skipping bound checking for file `$sourceFilePath`")
      None
    }
  }

  /**
   *
   * @param decompositionResult Results from decomposition
   * @param arguments           Command line arguments
   */
  def checkBound(decompositionResult: Option[List[DecompositionResult]], arguments: CommandLineArguments): List[RawResult] = {
    logger.info(s"Phase 2: Bound check")

    decompositionResult match {
      case Some(decompositionResults) =>
        decompositionResults.zipWithIndex.map({
          case (result, index) =>
            logger.info("")
            logger.info("")
            logger.info(s"Check bound for `$index`-th decomposition result")
            val startTime = System.nanoTime()
            val inputMethod = result.inputMethod
            val solver: Z3Solver = new Z3Solver
            BoundChecking.ensureNoAssertion(inputMethod.methodTree)
            val boundExpression: AST = BoundChecking.extractBoundExpression(solver,
              inputMethod.methodTree, inputMethod.inputVariables ++ inputMethod.localVariables, arguments.getLessPreciseBound)
            logger.info(s"Extracted bound expression is `$boundExpression`")

            val verified = {
              if (arguments.getDecomposeOnly) {
                logger.info(s"Not perform bound check")
                false
              }
              else BoundChecking.checkBound(solver, result, boundExpression, arguments)
            }
            val endTime = System.nanoTime()
            val timeElapsed = (endTime - startTime).toDouble / 1000000000
            logger.info(s"Time consumption: `${StringFormatUtils.float(timeElapsed)}` seconds")
            val numberOfLines = result.inputMethod.sourceCode.split("\r\n|\r|\n").length
            RawResult(result.inputMethod.className, timeElapsed, verified, result.amortizationMode, numberOfLines)
        })
      case None => List[RawResult]()
    }
  }

  /**
   *
   * @param solver             Z3 solver
   * @param sourceFilePath     Used to extract class name
   * @param sourceFileContents Source code
   * @param arguments          Command line arguments
   * @return
   */
  @deprecated
  def inferResourceInvariants(solver: Z3Solver, sourceFilePath: String, sourceFileContents: String, arguments: CommandLineArguments): Option[GlobalInvariants] = {
    assert(arguments.getAmortizationMode != ALL_AMORTIZE, "Expect choosing one amortization mode")

    decompose(sourceFilePath, sourceFileContents, arguments) match {
      case Some(decompositionResults) =>
        val globalInvariants = decompositionResults.map({ result => BoundChecking.inferInvariantsForResource(solver, result, arguments) })
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
