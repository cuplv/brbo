package brbo.common

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import brbo.common.icra._
import com.microsoft.z3._
import org.apache.logging.log4j.LogManager

class Z3Solver { // Copied from hopper: https://github.com/cuplv/hopper
  private val context: Context = Z3Solver.createContext
  private val solver: Solver = Z3Solver.createSolverUnderContext(context)

  // private var variablesToASTs = new HashMap[String, Variable]

  private val TRUE = "true"
  private val FALSE = "false"
  private val SELF = "self"
  private val CHECK_SAT = "(check-sat)"
  private val GET_MODEL = "(get-model)"
  private val GET_OBJECTIVES = "(get-objectives)"
  private val INIT = "init"
  private val ASSERT = "assert"
  private val DECL_CONST = "declare-const"
  private val MAXIMIZE = "maximize"
  private val ASSERT_FALSE = "(assert false)"
  private val ASSERT_TRUE = "(assert true)"

  def checkSAT: Boolean = Z3Solver.solverCheck(solver)

  def push(): Unit = solver.push()

  def pop(): Unit = solver.pop()

  /*def getIntVariableAST(identifier: String): AST = {
    variablesToASTs.get(identifier) match {
      case Some(variable) => variable.ast
      case None =>
        val ast = mkIntVar(identifier)
        variablesToASTs += (identifier -> Variable(identifier, INT, ast))
        ast
    }
  }*/

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

  def mkOr(left: AST, right: AST): AST = context.mkOr(left.asInstanceOf[BoolExpr], right.asInstanceOf[BoolExpr])

  def mkXor(left: AST, right: AST): AST = context.mkXor(left.asInstanceOf[BoolExpr], right.asInstanceOf[BoolExpr])

  def mkIntVal(i: Int): AST = context.mkInt(i)

  def mkBoolVal(b: Boolean): AST = context.mkBool(b)

  def mkIntVar(s: String): AST = context.mkIntConst(s)

  def mkBoolVar(s: String): AST = context.mkBoolConst(s)

  def generateIntegerVariableDeclaration(identifier: String): String = "(" + DECL_CONST + " " + identifier + " Int)"

  def generateAssertion(assertion: String): String = {
    def addBrackets(str: String): String = {
      if (str.contains(" ") && !str.startsWith("("))
        "(" + str + ")"
      else
        str
    }

    "(" + ASSERT + " " + addBrackets(assertion) + ")"
  }

  /*def getAssertions: String = {
    // TODO: Should consider variables with other types
    val variableDeclarations = variablesToASTs.foldLeft("") { case (acc, (name, _)) => acc + generateIntegerVariableDeclaration(name) + "\n" }
    solver.getAssertions.foldLeft(variableDeclarations) { (acc, assertion) => acc + generateAssertion(assertion.toString) + "\n" }
  }*/
}

object Z3Solver {
  private val logger = LogManager.getLogger("brbo.common.Z3Solver")

  private var timeUsage: Double = 0
  private var numberOfQueries: Int = 0

  private val configuration = new java.util.HashMap[String, String]
  configuration.put("model", "true")

  def getTimeUsage: Double = timeUsage

  def getNumberOfQueries: Int = numberOfQueries

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