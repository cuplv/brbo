package brbo.common

import com.microsoft.z3._
import org.apache.logging.log4j.LogManager

import java.io.PrintWriter
import java.nio.file.Files
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future, TimeoutException, blocking}
import scala.sys.process.ProcessLogger
import scala.sys.process._
import scala.concurrent.ExecutionContext.Implicits.global

class Z3Solver {
  // Copied from hopper: https://github.com/cuplv/hopper
  // This is a thread safe version
  // Otherwise, inferring invariants in parallel would break, due to concurrently invoking Z3 APIs
  private val logger = Z3Solver.logger

  Z3Solver.loadNativeLibraries()

  private val context: Context = Z3Solver.createContext
  private val solver: Solver = Z3Solver.createSolverUnderContext(context)

  def checkSAT(printUnsatCore: Boolean): Boolean = this.synchronized {
    Z3Solver.solverCheck(solver, printUnsatCore)
  }

  def checkAssertionPushPop(ast: AST, printUnsatCore: Boolean): Boolean = this.synchronized {
    push()
    mkAssert(ast)
    val result = checkSAT(printUnsatCore)
    pop()
    result
  }

  def getAssertions: Array[BoolExpr] = this.synchronized {
    solver.getAssertions
  }

  def push(): Unit = this.synchronized {
    solver.push()
  }

  def pop(): Unit = this.synchronized {
    solver.pop()
  }

  def mkAssert(assertion: AST): Unit = this.synchronized {
    solver.add(assertion.asInstanceOf[BoolExpr])
  }

  def mkNot(assertion: AST): BoolExpr = this.synchronized {
    context.mkNot(assertion.asInstanceOf[BoolExpr])
  }

  def mkEq(left: AST, right: AST): BoolExpr = this.synchronized {
    context.mkEq(left.asInstanceOf[Expr], right.asInstanceOf[Expr])
  }

  def mkNe(left: AST, right: AST): BoolExpr = this.synchronized {
    context.mkNot(context.mkEq(left.asInstanceOf[Expr], right.asInstanceOf[Expr]))
  }

  def mkGt(left: AST, right: AST): BoolExpr = this.synchronized {
    context.mkGt(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])
  }

  def mkLt(left: AST, right: AST): BoolExpr = this.synchronized {
    context.mkLt(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])
  }

  def mkGe(left: AST, right: AST): BoolExpr = this.synchronized {
    context.mkGe(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])
  }

  def mkLe(left: AST, right: AST): BoolExpr = this.synchronized {
    context.mkLe(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])
  }

  def mkAdd(left: AST, right: AST): ArithExpr = this.synchronized {
    context.mkAdd(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])
  }

  def mkAdd(astSequence: AST*): ArithExpr = this.synchronized {
    if (astSequence.isEmpty) mkIntVal(0)
    else context.mkAdd(astSequence.map(ast => ast.asInstanceOf[ArithExpr]): _*)
  }

  def mkSub(left: AST, right: AST): ArithExpr = this.synchronized {
    context.mkSub(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])
  }

  def mkMul(left: AST, right: AST): ArithExpr = this.synchronized {
    context.mkMul(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])
  }

  def mkDiv(left: AST, right: AST): ArithExpr = this.synchronized {
    context.mkDiv(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])
  }

  def mkMod(left: AST, right: AST): IntExpr = this.synchronized {
    context.mkMod(left.asInstanceOf[IntExpr], right.asInstanceOf[IntExpr])
  }

  def mkImplies(left: AST, right: AST): BoolExpr = this.synchronized {
    context.mkImplies(left.asInstanceOf[BoolExpr], right.asInstanceOf[BoolExpr])
  }

  def mkAnd(left: AST, right: AST): BoolExpr = this.synchronized {
    context.mkAnd(left.asInstanceOf[BoolExpr], right.asInstanceOf[BoolExpr])
  }

  def mkAnd(astSequence: AST*): BoolExpr = this.synchronized {
    if (astSequence.isEmpty) {
      logger.fatal("Attempting to conjoin empty AST")
      mkTrue()
    }
    else context.mkAnd(astSequence.map(ast => ast.asInstanceOf[BoolExpr]): _*)
  }

  def mkOr(left: AST, right: AST): BoolExpr = this.synchronized {
    context.mkOr(left.asInstanceOf[BoolExpr], right.asInstanceOf[BoolExpr])
  }

  def mkOr(astSequence: AST*): BoolExpr = this.synchronized {
    if (astSequence.isEmpty) throw new Exception("Attempting to disjoin empty AST")
    else context.mkOr(astSequence.map(ast => ast.asInstanceOf[BoolExpr]): _*)
  }

  def mkXor(left: AST, right: AST): BoolExpr = this.synchronized {
    context.mkXor(left.asInstanceOf[BoolExpr], right.asInstanceOf[BoolExpr])
  }

  def mkIntVal(i: Int): IntNum = this.synchronized {
    context.mkInt(i)
  }

  def mkBoolVal(b: Boolean): BoolExpr = this.synchronized {
    context.mkBool(b)
  }

  def mkIntVar(s: String): IntExpr = this.synchronized {
    context.mkIntConst(s)
  }

  def mkBoolVar(s: String): BoolExpr = this.synchronized {
    context.mkBoolConst(s)
  }

  def mkExists(boundConstants: Iterable[AST], body: AST): Quantifier = this.synchronized {
    /**
     * Weight annotations to quantifiers influence the priority of quantifier
     * instantiations.  They should be handled with care for solvers, which support
     * them, because incorrect choices of weights might render a problem unsolvable.
     *
     * Weights must be non-negative.  The value @{text 0} is equivalent to providing
     * no weight at all.
     *
     * Weights should only be used at quantifiers and only inside triggers (if the
     * quantifier has triggers).
     *
     * https://www.isa-afp.org/browser_info/Isabelle2013/HOL/SMT.html
     *
     * Weights are specific to Z3. The greater the weight of the quantifier, the fewer instantiations are allowed.
     * The instantiations that take place are those by terms that became active early, because they are more likely
     * to be relevant to the problem at hand. Sledgehammer’s iterative relevance filter yields a list of facts sorted
     * by likely relevance. This gives an easy way for Sledgehammer to fill in the weights meaning-fully: Give a weight
     * of 0 to the most relevant fact included, N to the least relevant fact,and interpolate in between. The case N=0
     * corresponds to Z3’s default behavior. We use N=10 with a quadratic interpolation, which appears to help more
     * than it harms.
     *
     * http://people.mpi-inf.mpg.de/~jblanche/jar-smt.pdf
     */
    context.mkExists(boundConstants.map(ast => ast.asInstanceOf[Expr]).toArray, body.asInstanceOf[Expr], 0, null, null, null, null)
  }

  def mkForall(boundConstants: Iterable[AST], body: AST): Quantifier = this.synchronized {
    context.mkForall(boundConstants.map(ast => ast.asInstanceOf[Expr]).toArray, body.asInstanceOf[Expr], 0, null, null, null, null)
  }

  def mkTrue(): BoolExpr = this.synchronized {
    mkBoolVal(true)
  }

  def mkFalse(): BoolExpr = this.synchronized {
    mkBoolVal(false)
  }

  def mkITE(condition: AST, trueCase: AST, falseExpr: AST): Expr = this.synchronized {
    context.mkITE(
      condition.asInstanceOf[BoolExpr],
      trueCase.asInstanceOf[Expr],
      falseExpr.asInstanceOf[Expr]
    )
  }

  def printAssertions(): Unit = this.synchronized {
    logger.error("Assertions are:")
    solver.getAssertions.foreach(expression => println(expression))
  }

  def printModel(): Unit = this.synchronized {
    logger.error(s"Model is:\n${solver.getModel.toString}")
  }
}

object Z3Solver {
  private val logger = LogManager.getLogger("brbo.common.Z3Solver")

  private val configuration = new java.util.HashMap[String, String]
  configuration.put("model", "true")
  private val Z3_COMMAND_LINE_TIMEOUT = 10 // Unit: Seconds

  val Z3_PATH: String = s"${System.getProperty("user.dir")}/lib/z3/z3"

  def toSmt2File(solver: Z3Solver): String = solver.solver.toString

  def checkSATCommandLine(solver: Z3Solver): Boolean = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val smt2FileContents = toSmt2File(solver) + "\n(check-sat)"
    val file = Files.createTempFile("prefix-", ".smt2")
    new PrintWriter(file.toAbsolutePath.toString) {
      write(smt2FileContents);
      close()
    }

    val cmd = s"$Z3_PATH -T:$Z3_COMMAND_LINE_TIMEOUT -smt2 ${file.toAbsolutePath}"

    try {
      val process = cmd.run(ProcessLogger(stdout append _, stderr append _))
      val future = Future(blocking(process.exitValue()))
      val actualTimeout = Duration(Z3_COMMAND_LINE_TIMEOUT, SECONDS)
      val result = try {
        Await.result(future, actualTimeout)
      } catch {
        case _: TimeoutException =>
          logger.fatal(s"Z3 timed out after `$actualTimeout`!")
          process.destroy()
          process.exitValue()
      }
      if (result == 0) {
        val removeFile = s"rm $file"
        removeFile.!!
        stdout.toString() match {
          case "unsat" => false
          case "sat" => true
          case _ =>
            logger.info(s"Z3 stdout:\n$stdout")
            throw new Exception("Unknown Z3 output")
        }
      }
      else {
        logger.fatal(s"Error when running Z3. Exit code: `$result`")
        logger.error(s"stderr:\n$stderr")
        false
      }
    }
    catch {
      case e: Exception =>
        logger.error(s"Exception when executing command `$cmd`", e)
        logger.error(s"stdout:\n$stdout")
        logger.error(s"stderr:\n$stderr")
        throw new RuntimeException("Error when running Z3")
    }
  }

  // Run this before creating an instance of Z3Solver
  def loadNativeLibraries(): Unit = {
    logger.trace(System.getProperty("java.library.path"))
    logger.trace(System.mapLibraryName("z3"))
    logger.trace(System.mapLibraryName("z3java"))
    System.loadLibrary("z3")
    System.loadLibrary("z3java")
  }

  private def createSolverUnderContext(context: Context): Solver = {
    val solver = context.mkSolver
    val parameters = context.mkParams()
    parameters.add("timeout", 10000)
    parameters.add("unsat_core", true)
    solver.setParameters(parameters)
    solver
  }

  private def createContext: Context = new Context(configuration)

  private def solverCheck(solver: Solver, printUnsatCore: Boolean): Boolean = {
    // val start = System.nanoTime()
    val result = {
      solver.check() match {
        case Status.UNSATISFIABLE =>
          if (printUnsatCore) {
            logger.error("Unsat core is:")
            solver.getUnsatCore.foreach(expression => println(expression))
            // FYI: https://stackoverflow.com/questions/18132243/get-unsat-core-returns-empty-in-z3
          }
          false
        case Status.SATISFIABLE => true
        case Status.UNKNOWN => throw Z3UnknownException(s"`${Status.UNKNOWN}` - Reason: `${solver.getReasonUnknown}`")
      }
    }
    // val end = System.nanoTime()
    // Z3Solver.timeUsage += (end - start).toDouble / 1000000000
    // Z3Solver.numberOfQueries += 1
    result
  }

  /*def check(assertion: BoolExpr, debug: Boolean): Boolean = {
    val context = new Context
    val solver = createSolverUnderContext(context)
    solver.add(assertion)
    solverCheck(solver, debug)
  }*/

  def parseSMTLIB2StringToArray(string: String, context: Context): Array[BoolExpr] = {
    try {
      context.parseSMTLIB2String(string, null, null, null, null)
    } catch {
      case e: Exception =>
        logger.error(s"SMTLIB2 string parse exception:\n$string\n", e)
        throw new RuntimeException("SMTLIB2 string parse exception")
    }
  }

  def parseSMTLIB2String(string: String, context: Context): BoolExpr = {
    val array = parseSMTLIB2StringToArray(string, context)
    if (array.length == 1) array.head
    else if (array.isEmpty) context.mkTrue()
    else context.mkAnd(parseSMTLIB2StringToArray(string, context): _*)
  }

}