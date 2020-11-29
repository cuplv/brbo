package brbo

import brbo.boundinference.BoundInferenceProcessor
import brbo.common.JavacUtils
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
    logger.info(s"Reading locations of source files in `${args(0)}`")
    val sourceFiles: Map[String, String] = {
      readFromFile(args(0)).split("\n").foldLeft(new HashMap[String, String])({
        (acc, sourceFileLocation) =>
          logger.info(s"Reading from source file `$sourceFileLocation`")
          acc + (sourceFileLocation -> readFromFile(sourceFileLocation))
      })
    }

    sourceFiles.foreach({
      case (sourceFileLocation, sourceFileContents) =>
        logger.info(s"Inferring bound for file `$sourceFileLocation`")
        val boundInferenceProcessor = new BoundInferenceProcessor(FilenameUtils.getName(sourceFileLocation), sourceFileContents)
        JavacUtils.runProcessor(FilenameUtils.getBaseName(sourceFileLocation), sourceFileContents, boundInferenceProcessor)
        boundInferenceProcessor.runAnalysis()
    })
  }
}
