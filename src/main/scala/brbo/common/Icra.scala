package brbo.common

import java.io.PrintWriter
import java.nio.file.Files

import org.apache.logging.log4j.LogManager

import scala.sys.process.{ProcessLogger, _}

object Icra {
  private val logger = LogManager.getLogger("brbo.common.Icra")
  private val icraPath = s"${System.getProperty("user.home")}/Documents/workspace/icra/icra"

  def run(sourceCode: String): Unit = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val file = Files.createTempFile("prefix-", ".c")
    new PrintWriter(file.toAbsolutePath.toString) {
      write(sourceCode); close()
    }

    val cmd = s"$icraPath ${file.toAbsolutePath}"

    try {
      val status = cmd ! ProcessLogger(stdout append _, stderr append _)
      if (status == 0) {
        logger.info(s"stdout:\n$stdout")
      }
      else {
        throw new RuntimeException("Error when running ICRA")
      }
    }
    catch {
      case e: Exception =>
        logger.error(s"Exception when executing command `$cmd`", e)
        logger.error(s"stdout:\n$stdout")
        logger.error(s"stderr:\n$stderr")
        throw new RuntimeException("Error when running ICRA")
    }
    finally {
      val removeFile = s"rm $file"
      removeFile.!!
    }
  }
}
