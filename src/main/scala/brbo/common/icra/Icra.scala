package brbo.common.icra

import java.io.PrintWriter
import java.nio.file.Files

import brbo.common.TypeUtils.BrboType._
import brbo.common.Z3Solver
import com.microsoft.z3.AST
import org.apache.logging.log4j.LogManager

import scala.collection.immutable.HashSet
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.{Duration, SECONDS}
import scala.sys.process._

object Icra {
  private val logger = LogManager.getLogger("brbo.common.icra.Icra")
  val ICRA_PATH = s"${System.getProperty("user.home")}/Documents/workspace/icra/icra"

  def runAndParseInvariant(sourceCode: String, timeout: Int, icraPath: String): Option[List[ParsedInvariant]] = {
    runAndGetStdOutput(sourceCode, timeout, icraPath) match {
      case Some(icraOutput) => Some(parseInvariants(icraOutput))
      case None => None
    }
  }

  def runAndParseAssertionChecks(sourceCode: String, timeout: Int, icraPath: String): Option[List[Boolean]] = {
    runAndGetStdOutput(sourceCode, timeout, icraPath) match {
      case Some(icraOutput) => Some(parseAssertionChecks(icraOutput))
      case None => None
    }
  }

  private def runAndGetStdOutput(sourceCode: String, timeout: Int, icraPath: String): Option[String] = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val file = Files.createTempFile("prefix-", ".c")
    new PrintWriter(file.toAbsolutePath.toString) {
      write(sourceCode);
      close()
    }

    // val cmd = s"""bash -c \"ulimit -H -v 1000000 && $ICRA_PATH -cra-split-loops -cra-prsd ${file.toAbsolutePath}\""""
    // val cmd = s"""ulimit -H -v 1000000; $ICRA_PATH -cra-split-loops -cra-prsd ${file.toAbsolutePath}"""
    val cmd = {
      val timeoutPrefix = if (timeout >= 0) s"timeout ${timeout}s" else ""
      s"$timeoutPrefix $icraPath -cra-split-loops -cra-prsd ${file.toAbsolutePath}"
    }

    try {
      // Set a timeout
      // val status = cmd ! ProcessLogger(stdout append _, stderr append _)
      val process = cmd.run(ProcessLogger(stdout append _, stderr append _))
      val future = Future(blocking(process.exitValue())) // wrap in Future
      val actualTimeout = {
        if (timeout >= 0) Duration(timeout, SECONDS)
        else Duration.Inf
      }
      val result = try {
        Await.result(future, actualTimeout)
      } catch {
        case _: TimeoutException =>
          logger.fatal(s"ICRA timed out after `$actualTimeout`!")
          process.destroy()
          process.exitValue()
      }
      if (result == 0) {
        logger.trace(s"ICRA stdout:\n$stdout")
        val removeFile = s"rm $file"
        removeFile.!!
        Some(stdout.toString())
      }
      else {
        logger.fatal(s"Error when running ICRA. Exit code: `$result`")
        logger.error(s"stderr:\n$stderr")
        None
      }
    }
    catch {
      case e: Exception =>
        logger.error(s"Exception when executing command `$cmd`", e)
        logger.error(s"stdout:\n$stdout")
        logger.error(s"stderr:\n$stderr")
        throw new RuntimeException("Error when running ICRA")
    }
  }

  def parseInvariants(icraOutput: String): List[ParsedInvariant] = {
    val parser = new IcraParser(icraOutput)
    parser.extractRawInvariants.map({
      rawInvariant => parser.parseRawInvariant(rawInvariant)
    })
  }

  def parseAssertionChecks(icraOutput: String): List[Boolean] = {
    val lines = icraOutput.split("\n")
    val result = lines.foldLeft(List[Boolean]())({
      (acc, line) =>
        if (line.contains("(Assertion on line")) {
          if (line.contains("FAILED")) {
            false :: acc
          }
          else if (line.contains("PASSED")) {
            true :: acc
          }
          else throw new Exception(s"Unexpected ICRA output: $line")
        }
        else acc
    })
    result.reverse
  }

  def translateToZ3(icraAST: IcraAST, typ: BrboType, solver: Z3Solver): AST = translateToZ3AndCollectVariables(icraAST, typ, solver)._1

  def translateToZ3AndCollectVariables(icraAST: IcraAST, typ: BrboType, solver: Z3Solver): (AST, Set[String]) = {
    var variables = new HashSet[String]

    def helper(icraAST: IcraAST, typ: BrboType, solver: Z3Solver): AST = {
      icraAST match {
        case Identifier(identifier) =>
          typ match {
            case INT =>
              variables = variables + identifier
              solver.mkIntVar(identifier)
            case BOOL =>
              identifier match {
                case "true" => solver.mkTrue()
                case "false" => solver.mkFalse()
                case _ => throw new RuntimeException(s"Did not expect ICRA to define bool-typed variable $identifier in invariants!")
              }
          }
        case Number(number) => solver.mkIntVal(number)
        case Addition(left, right) => solver.mkAdd(helper(left, INT, solver), helper(right, INT, solver))
        case Subtraction(left, right) => solver.mkSub(helper(left, INT, solver), helper(right, INT, solver))
        case Multiplication(left, right) => solver.mkMul(helper(left, INT, solver), helper(right, INT, solver))
        case Division(left, right) => solver.mkDiv(helper(left, INT, solver), helper(right, INT, solver))
        case Negative(expression) => solver.mkSub(solver.mkIntVal(0), helper(expression, INT, solver))
        case LessThan(left, right) => solver.mkLt(helper(left, INT, solver), helper(right, INT, solver))
        case LessThanOrEqualTo(left, right) => solver.mkLe(helper(left, INT, solver), helper(right, INT, solver))
        case GreaterThan(left, right) => solver.mkGt(helper(left, INT, solver), helper(right, INT, solver))
        case GreaterThanOrEqualTo(left, right) => solver.mkGe(helper(left, INT, solver), helper(right, INT, solver))
        case Equal(left, right) => solver.mkEq(helper(left, INT, solver), helper(right, INT, solver))
        case And(left, right) => solver.mkAnd(helper(left, BOOL, solver), helper(right, BOOL, solver))
        case Or(left, right) => solver.mkOr(helper(left, BOOL, solver), helper(right, BOOL, solver))
        case Negation(expression) => solver.mkNot(helper(expression, BOOL, solver))
        case IfThenElse(condition, left, right) => solver.mkITE(helper(condition, BOOL, solver), helper(left, INT, solver), helper(right, INT, solver))
        case _ => throw new RuntimeException(s"IcraAST should never contain $icraAST after parsing")
      }
    }

    (helper(icraAST, typ, solver), variables)
  }
}
