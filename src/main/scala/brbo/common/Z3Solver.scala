package brbo.common

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType}
import brbo.common.icra._
import com.microsoft.z3._
import org.apache.logging.log4j.LogManager

class Z3Solver { // Copied from hopper: https://github.com/cuplv/hopper
  Z3Solver.loadNativeLibraries()

  private val context: Context = Z3Solver.createContext
  private val solver: Solver = Z3Solver.createSolverUnderContext(context)

  def checkSAT: Boolean = Z3Solver.solverCheck(solver)

  def push(): Unit = solver.push()

  def pop(): Unit = solver.pop()

  def mkAssert(assertion: AST): Unit = solver.add(assertion.asInstanceOf[BoolExpr])

  def mkNot(assertion: AST): AST = context.mkNot(assertion.asInstanceOf[BoolExpr])

  def mkEq(left: AST, right: AST): AST = context.mkEq(left.asInstanceOf[Expr], right.asInstanceOf[Expr])

  def mkNe(left: AST, right: AST): AST = context.mkNot(context.mkEq(left.asInstanceOf[Expr], right.asInstanceOf[Expr]))

  def mkGt(left: AST, right: AST): AST = context.mkGt(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])

  def mkLt(left: AST, right: AST): AST = context.mkLt(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])

  def mkGe(left: AST, right: AST): AST = context.mkGe(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])

  def mkLe(left: AST, right: AST): AST = context.mkLe(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])

  def mkAdd(left: AST, right: AST): AST = context.mkAdd(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])

  def mkAdd(astSequence: AST*): AST = {
    if (astSequence.isEmpty) mkIntVal(0)
    else context.mkAdd(astSequence.map(ast => ast.asInstanceOf[ArithExpr]): _*)
  }

  def mkSub(left: AST, right: AST): AST = context.mkSub(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])

  def mkMul(left: AST, right: AST): AST = context.mkMul(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])

  def mkDiv(left: AST, right: AST): AST = context.mkDiv(left.asInstanceOf[ArithExpr], right.asInstanceOf[ArithExpr])

  def mkMod(left: AST, right: AST): AST = context.mkMod(left.asInstanceOf[IntExpr], right.asInstanceOf[IntExpr])

  def mkImplies(left: AST, right: AST): AST = context.mkImplies(left.asInstanceOf[BoolExpr], right.asInstanceOf[BoolExpr])

  def mkAnd(left: AST, right: AST): AST = context.mkAnd(left.asInstanceOf[BoolExpr], right.asInstanceOf[BoolExpr])

  def mkAnd(astSequence: AST*): AST = {
    if (astSequence.isEmpty) throw new Exception("Attempting to conjoin empty AST")
    else context.mkAnd(astSequence.map(ast => ast.asInstanceOf[BoolExpr]): _*)
  }

  def mkOr(left: AST, right: AST): AST = context.mkOr(left.asInstanceOf[BoolExpr], right.asInstanceOf[BoolExpr])

  def mkOr(astSequence: AST*): AST = {
    if (astSequence.isEmpty) throw new Exception("Attempting to disjoin empty AST")
    else context.mkAnd(astSequence.map(ast => ast.asInstanceOf[BoolExpr]): _*)
  }

  def mkXor(left: AST, right: AST): AST = context.mkXor(left.asInstanceOf[BoolExpr], right.asInstanceOf[BoolExpr])

  def mkIntVal(i: Int): AST = context.mkInt(i)

  def mkBoolVal(b: Boolean): AST = context.mkBool(b)

  def mkIntVar(s: String): AST = context.mkIntConst(s)

  def mkBoolVar(s: String): AST = context.mkBoolConst(s)

  def mkExists(boundConstants: Iterable[Expr], body: Expr): AST = {
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
    context.mkExists(boundConstants.toArray, body, 0, null, null, null, null)
  }

  def mkForall(boundConstants: Iterable[Expr], body: Expr): AST = {
    context.mkForall(boundConstants.toArray, body, 0, null, null, null, null)
  }

  def mkTrue(): AST = mkBoolVal(true)

  def mkFalse(): AST = mkBoolVal(false)
}

object Z3Solver {
  private val logger = LogManager.getLogger("brbo.common.Z3Solver")

  private var timeUsage: Double = 0
  private var numberOfQueries: Int = 0

  private val configuration = new java.util.HashMap[String, String]
  configuration.put("model", "true")

  def getTimeUsage: Double = timeUsage

  def getNumberOfQueries: Int = numberOfQueries

  // Run this before creating an instance of Z3Solver
  def loadNativeLibraries(): Unit = {
    logger.debug(System.getProperty("java.library.path"))
    logger.debug(System.mapLibraryName("z3"))
    logger.debug(System.mapLibraryName("z3java"))
    System.loadLibrary("z3")
    System.loadLibrary("z3java")
  }

  private def createSolverUnderContext(context: Context): Solver = {
    val solver = context.mkSolver
    val parameters = context.mkParams()
    parameters.add("timeout", 10000)
    solver.setParameters(parameters)
    solver
  }

  private def createContext: Context = new Context(configuration)

  private def solverCheck(solver: Solver): Boolean = {
    val start = System.nanoTime()
    val result = {
      solver.check() match {
        case Status.UNSATISFIABLE => false
        case Status.SATISFIABLE => true
        case Status.UNKNOWN => throw new RuntimeException("Status.UNKNOWN: Z3 decidability or timeout issue")
      }
    }
    val end = System.nanoTime()
    Z3Solver.timeUsage += (end - start).toDouble / 1000000000
    Z3Solver.numberOfQueries += 1
    result
  }

  def assert(icraAST: IcraAST, solver: Z3Solver): Unit = {
    solver.mkAssert(Icra.translateToZ3(icraAST, BOOL, solver))
  }

  def check(assertion: BoolExpr): Boolean = {
    val context = new Context
    val solver = createSolverUnderContext(context)
    solver.add(assertion)
    solverCheck(solver)
  }

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

  case class Variable(identifier: String, typ: BrboType, ast: AST)

}