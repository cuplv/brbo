package brbo.common.icra

import java.io.PrintWriter
import java.nio.file.Files

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import brbo.common.Z3Solver
import com.microsoft.z3.AST
import org.apache.logging.log4j.LogManager

import scala.sys.process._

object Icra {
  private val logger = LogManager.getLogger("brbo.common.icra.Icra")
  private val icraPath = s"${System.getProperty("user.home")}/Documents/workspace/icra/icra"

  def run(sourceCode: String): Unit = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val file = Files.createTempFile("prefix-", ".c")
    new PrintWriter(file.toAbsolutePath.toString) {
      write(sourceCode);
      close()
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

  def parseInvariants(icraOutput: String): Set[AST] = {
    ???
  }

  def translateToZ3(icraAST: IcraAST, typ: BrboType, solver: Z3Solver): AST = {
    icraAST match {
      case EmptyAST => throw new RuntimeException(s"IcraAST should never contain $EmptyAST after parsing")
      case Identifier(identifier) =>
        typ match {
          case INT => solver.mkIntVar(identifier)
          case BOOL => solver.mkBoolVar(identifier)
        }
      case Number(number) => solver.mkIntVal(number)
      case Addition(left, right) => solver.mkAdd(translateToZ3(left, INT, solver), translateToZ3(right, INT, solver))
      case Subtraction(left, right) => solver.mkSub(translateToZ3(left, INT, solver), translateToZ3(right, INT, solver))
      case Multiplication(left, right) => solver.mkMul(translateToZ3(left, INT, solver), translateToZ3(right, INT, solver))
      case Division(left, right) => solver.mkDiv(translateToZ3(left, INT, solver), translateToZ3(right, INT, solver))
      case Negative(expression) => solver.mkSub(solver.mkIntVal(0), translateToZ3(expression, INT, solver))
      case LessThan(left, right) => solver.mkLt(translateToZ3(left, INT, solver), translateToZ3(right, INT, solver))
      case LessThanOrEqualTo(left, right) => solver.mkLe(translateToZ3(left, INT, solver), translateToZ3(right, INT, solver))
      case GreaterThan(left, right) => solver.mkGt(translateToZ3(left, INT, solver), translateToZ3(right, INT, solver))
      case GreaterThanOrEqualTo(left, right) => solver.mkGe(translateToZ3(left, INT, solver), translateToZ3(right, INT, solver))
      case Equal(left, right) =>
        var leftTyp = INT
        var leftAST: Option[AST] = None
        try {
          leftAST = Some(translateToZ3(left, INT, solver))
        } catch {
          case _: MatchError =>
            leftAST = Some(translateToZ3(left, BOOL, solver))
            leftTyp = BOOL
        }
        // solver.mkEq(, translateToZ3(right, INT, solver))
        ???
      case And(left, right) => solver.mkAnd(translateToZ3(left, BOOL, solver), translateToZ3(right, BOOL, solver))
      case Or(left, right) => solver.mkOr(translateToZ3(left, BOOL, solver), translateToZ3(right, BOOL, solver))
      case Negation(expression) => solver.mkNot(translateToZ3(expression, BOOL, solver))
    }
  }
}
