package brbo.common

import brbo.common.CommandLineArguments.logger
import brbo.common.icra.Icra
import brbo.verification.AmortizationMode._
import org.apache.logging.log4j.LogManager
import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import scala.collection.JavaConverters._

class CommandLineArguments {

  @Option(name = "--amortize", aliases = Array("-a"), required = false,
    usage = "The mode of amortization. Choose from: `FULL`, `NO`, `SELECTIVE`, `ALL` (case-insensitive)")
  private var amortizationMode: String = "selective"

  @Option(name = "--debug", aliases = Array("-g"), required = false,
    usage = "Turn on the debug mode.")
  private var debugMode: Boolean = false

  @Option(name = "--skip-sanity-check", aliases = Array("-s"), required = false,
    usage = "Skip the sanity check.")
  private var skipSanityCheck: Boolean = false

  @Option(name = "--print-counter-example", aliases = Array("--cex"), required = false,
    usage = "Print a counter example that violates the bound.")
  private var printCounterExample: Boolean = false

  @Option(name = "--print-icra-inputs", aliases = Array("-i"), required = false,
    usage = "Print input programs to ICRA.")
  private var printIcraInputs: Boolean = false

  @Option(name = "--directory", aliases = Array("-d"), required = true,
    usage = "The directory to analyze.")
  private var directoryToAnalyze: String = "."

  @Option(name = "--icra-timeout", aliases = Array("-t"),
    usage = "The amount of timeout (unit: seconds) allowed for each invocation to ICRA. Negative numbers mean no timeout will be set.")
  private var icraTimeout: Int = CommandLineArguments.DEFAULT_ICRA_TIME_OUT

  @Option(name = "--print-cfg", aliases = Array("--cfg"), required = false, usage = "Print the control flow graph of the input graph.")
  private var printCFG: Boolean = false

  @Option(name = "--decompose-only", aliases = Array("--decompose"), required = false, usage = "Only perform decompose.")
  private var decomposeOnly: Boolean = false

  @Option(name = "--less-precise", required = false, usage = "Verify the bounds specified by function `lessPreciseBound`.")
  private var lessPreciseBound: Boolean = false

  @Option(name = "--generate-synthetic", required = false, usage = "Generate `n` synthetic programs")
  private var generateSynthetic: Int = 0

  @Option(name = "--icra-path", required = false,
    usage = "The absolute path of binary file `icra`.")
  private var icraPath: String = Icra.ICRA_PATH

  def getAmortizationMode: AmortizationMode = {
    amortizationMode.toLowerCase() match {
      case "no" => NO_AMORTIZE
      case "full" => FULL_AMORTIZE
      case "selective" => SELECTIVE_AMORTIZE
      case "all" => ALL_AMORTIZE
      case "unknown" => UNKNOWN
    }
  }

  def getDebugMode: Boolean = debugMode

  def getDirectoryToAnalyze: String = directoryToAnalyze

  def getSkipSanityCheck: Boolean = skipSanityCheck

  def getPrintCounterExample: Boolean = printCounterExample

  def getPrintIcraInputs: Boolean = printIcraInputs

  def getIcraTimeout: Int = icraTimeout

  def getPrintCFG: Boolean = printCFG

  def getDecomposeOnly: Boolean = decomposeOnly

  def getLessPreciseBound: Boolean = lessPreciseBound

  def getGenerateSynthetic: Int = generateSynthetic

  def getIcraPath: String = icraPath

  private var initialized = false

  def initialize(amortizationMode: AmortizationMode,
                 debugMode: Boolean,
                 directoryToAnalyze: String,
                 skipSanityCheck: Boolean,
                 printCounterExample: Boolean,
                 printIcraInputs: Boolean,
                 icraTimeout: Int,
                 printCFG: Boolean,
                 decomposeOnly: Boolean,
                 lessPreciseBound: Boolean,
                 generateSynthetic: Int,
                 icraPath: String): Unit = {
    if (initialized) {
      logger.info(s"Already initialized")
      return
    }
    initialized = true
    this.amortizationMode = amortizationModeToShortString(amortizationMode)
    this.debugMode = debugMode
    this.directoryToAnalyze = directoryToAnalyze
    this.skipSanityCheck = skipSanityCheck
    this.printCounterExample = printCounterExample
    this.printIcraInputs = printIcraInputs
    this.icraTimeout = icraTimeout
    this.printCFG = printCFG
    this.decomposeOnly = decomposeOnly
    this.lessPreciseBound = lessPreciseBound
    this.generateSynthetic = generateSynthetic
    this.icraPath = icraPath
  }

  override def toString: String = {
    val strings = List[String](
      s"Infer resource usage upper bounds for each method in each file `*.java` under directory `$directoryToAnalyze`",
      s"Amortization mode: `$getAmortizationMode`",
      s"Debug mode? `$debugMode`",
      s"Skip sanity check? `$skipSanityCheck`",
      s"Print counter examples if cannot verify the bound? `$printCounterExample`",
      s"Print inputs to ICRA? `$printIcraInputs`",
      s"ICRA's time out: `$icraTimeout` seconds",
      s"Print CFG? `$printCFG`",
      s"No bound check? `$decomposeOnly`",
      s"Check less precise bounds? `$lessPreciseBound`",
      s"Generate `$generateSynthetic` synthetic programs",
      s"Icra path is `$icraPath`"
    )
    strings.mkString("\n")
  }

  def toFileName: String = {
    val amortizationMode = amortizationModeToShortString(getAmortizationMode)
    val timeout = if (icraTimeout < 0) "noTimeout" else s"${icraTimeout}s"
    val boundPrecision = s"${if (lessPreciseBound) "less" else "most"}Precise"
    s"""$amortizationMode-$timeout-$boundPrecision"""
  }
}

object CommandLineArguments {
  private val logger = LogManager.getLogger("brbo.common.CommandLineArguments")

  def parseArguments(args: Array[String]): CommandLineArguments = {
    val arguments = new CommandLineArguments
    val parser = new CmdLineParser(arguments)
    try {
      parser.parseArgument(args.toList.asJava)
    } catch {
      case e: CmdLineException =>
        logger.fatal(s"Error:${e.getMessage}\nUsage:\n")
        parser.printUsage(System.out)
        System.exit(1)
    }
    arguments
  }

  val DEFAULT_ICRA_TIME_OUT = 15 // Unit: Second

  val DEFAULT_ARGUMENTS: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(UNKNOWN, debugMode = false, "", skipSanityCheck = false,
      printCounterExample = false, printIcraInputs = false, icraTimeout = 20,
      printCFG = false, decomposeOnly = false, lessPreciseBound = false, generateSynthetic = 0, icraPath = Icra.ICRA_PATH)
    arguments
  }
}