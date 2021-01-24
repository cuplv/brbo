package brbo.common.icra

import java.io.PrintWriter
import java.nio.file.Files

import brbo.common.TypeUtils.BrboType._
import brbo.common.Z3Solver
import com.microsoft.z3.AST
import org.apache.logging.log4j.LogManager

import scala.collection.immutable.HashSet
import scala.concurrent.{Await, Future, TimeoutException, blocking, duration}
import scala.sys.process._
import scala.concurrent.ExecutionContext.Implicits.global

object Icra {
  private val logger = LogManager.getLogger("brbo.common.icra.Icra")
  private val icraPath = s"${System.getProperty("user.home")}/Documents/workspace/icra/icra"
  private val TIMEOUT = 30 // Unit: Seconds

  def run(sourceCode: String): Option[List[ParsedInvariant]] = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val file = Files.createTempFile("prefix-", ".c")
    new PrintWriter(file.toAbsolutePath.toString) {
      write(sourceCode);
      close()
    }

    val cmd = s"$icraPath -cra-split-loops -cra-prsd ${file.toAbsolutePath}"

    var parsedInvariants: Option[List[ParsedInvariant]] = None

    try {
      // Set a timeout
      // val status = cmd ! ProcessLogger(stdout append _, stderr append _)
      val process = cmd.run(ProcessLogger(stdout append _, stderr append _))
      val future = Future(blocking(process.exitValue())) // wrap in Future
      val result = try {
        Await.result(future, duration.Duration(TIMEOUT, "sec"))
      } catch {
        case _: TimeoutException =>
          logger.fatal(s"ICRA timed out after `$TIMEOUT` seconds!")
          process.destroy()
          process.exitValue()
      }
      if (result == 0) {
        logger.trace(s"ICRA stdout:\n$stdout")
        parsedInvariants = Some(parseInvariants(stdout.toString()))
        val removeFile = s"rm $file"
        removeFile.!!
      }
      else {
        logger.fatal(s"Error when running ICRA. Exit code: `$result`")
      }
    }
    catch {
      case e: Exception =>
        logger.error(s"Exception when executing command `$cmd`", e)
        logger.error(s"stdout:\n$stdout")
        logger.error(s"stderr:\n$stderr")
        throw new RuntimeException("Error when running ICRA")
    }

    parsedInvariants
  }

  def parseInvariants(icraOutput: String): List[ParsedInvariant] = {
    val parser = new IcraParser(icraOutput)
    parser.extractRawInvariants.map({
      rawInvariant => parser.parseRawInvariant(rawInvariant)
    })
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
