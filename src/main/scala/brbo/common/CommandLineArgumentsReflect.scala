package brbo.common

import brbo.verification.AmortizationMode._
import org.apache.logging.log4j.LogManager
import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import scala.collection.JavaConverters._

class CommandLineArgumentsReflect {

  @Option(name = "--amortize", aliases = Array("-a"), required = false,
    usage = "The mode of amortization. Choose from: `FULL`, `NO`, `SELECTIVE`, `ALL` (case-insensitive)")
  private var amortizationMode: String = "selective"

  @Option(name = "--debug", aliases = Array("-g"), required = false,
    usage = "Turn on the debug mode.")
  private var debugMode: Boolean = false

  @Option(name = "--skip-sanity-check", aliases = Array("-s"), required = false,
    usage = "Skip the sanity check.")
  private var skipSanityCheck: Boolean = false

  @Option(name = "--print-counter-example", aliases = Array("-c"), required = false,
    usage = "Print a counter example that violates the bound.")
  private var printCounterExample: Boolean = false

  @Option(name = "--print-icra-inputs", aliases = Array("-i"), required = false,
    usage = "Print input programs to ICRA.")
  private var printIcraInputs: Boolean = false

  @Option(name = "--directory", aliases = Array("-d"), required = true,
    usage = "The directory to analyze.")
  private var directoryToAnalyze: String = "."

  @Option(name = "--icra-timeout", aliases = Array("-t"),
    usage = "The amount of timeout (unit: seconds) allowed for each invocation to ICRA. `-1` means no timeout will be set.")
  private var icraTimeout: Int = CommandLineArguments.DEFAULT_ICRA_TIME_OUT

  @Option(name = "--print-cfg", required = false,
    usage = "Print the control flow graph of the input graph.")
  private var printCFG: Boolean = false

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

  def getSkipSanityCheck: Boolean = skipSanityCheck

  def getPrintCounterExample: Boolean = printCounterExample

  def getPrintIcraInputs: Boolean = printIcraInputs

  def getIcraTimeout: Int = icraTimeout

  def getPrintCFG: Boolean = printCFG
}

object CommandLineArgumentsReflect {
  private val logger = LogManager.getLogger("brbo.common.CommandLineArguments")

  def parseArguments(args: Array[String]): CommandLineArguments = {
    val arguments = new CommandLineArgumentsReflect
    val parser = new CmdLineParser(arguments)
    try {
      parser.parseArgument(args.toList.asJava)
    } catch {
      case e: CmdLineException =>
        logger.fatal(s"Error:${e.getMessage}\nUsage:\n")
        parser.printUsage(System.out)
        System.exit(1)
    }
    CommandLineArguments(
      arguments.getAmortizationMode, arguments.debugMode,
      arguments.directoryToAnalyze, arguments.skipSanityCheck,
      arguments.printCounterExample, arguments.printIcraInputs,
      arguments.icraTimeout, arguments.printCFG
    )
  }
}