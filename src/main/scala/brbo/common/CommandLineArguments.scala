package brbo.common

import brbo.verification.AmortizationMode.{AmortizationMode, FULL_AMORTIZE, NO_AMORTIZE, SELECTIVE_AMORTIZE, ALL_AMORTIZE}
import org.apache.logging.log4j.LogManager
import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import scala.collection.JavaConverters._

class CommandLineArguments {

  @Option(name = "--amortize", aliases = Array("-a"), required = false,
    usage = "The mode of amortization. Choose from: `FULL`, `NO`, `SELECTIVE`, `ALL` (case-insensitive)")
  private var amortizationMode: String = "selective"

  @Option(name = "--debug", aliases = Array("-g"), required = false,
    usage = "Turn on the debug mode")
  private var debugMode: Boolean = false

  @Option(name = "--directory", aliases = Array("-d"), required = true,
    usage = "The directory to analyze.")
  private var directoryToAnalyze: String = "."

  def getAmortizationMode: AmortizationMode = {
    amortizationMode.toLowerCase() match {
      case "no" => NO_AMORTIZE
      case "full" => FULL_AMORTIZE
      case "selective" => SELECTIVE_AMORTIZE
      case "all" => ALL_AMORTIZE
    }
  }

  def getDebugMode: Boolean = debugMode

  def getDirectoryToAnalyze: String = directoryToAnalyze
}

object CommandLineArguments {
  private val logger = LogManager.getLogger("brbo.common.CommandLineArguments")

  def parseArguments(args: Array[String]): CommandLineArguments = {
    val commandLineArguments = new CommandLineArguments
    val parser = new CmdLineParser(commandLineArguments)
    try {
      parser.parseArgument(args.toList.asJava)
    } catch {
      case e: CmdLineException =>
        logger.fatal(s"Error:${e.getMessage}\nUsage:\n")
        parser.printUsage(System.out)
        System.exit(1)
    }
    commandLineArguments
  }
}