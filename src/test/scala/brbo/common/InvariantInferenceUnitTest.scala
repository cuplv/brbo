package brbo.common

import brbo.common.BeforeOrAfterOrThis.{AFTER, BEFORE}
import brbo.common.GhostVariableUtils.GhostVariable.Delta
import brbo.common.TypeUtils.BrboType.{BrboType, INT}
import brbo.verification.{BasicProcessor, BoundCheckingUnitTest}
import brbo.{StringCompare, TestCaseJavaProgram}
import com.sun.source.tree.{ExpressionStatementTree, Tree, VariableTree}
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

class InvariantInferenceUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[BoundCheckingUnitTest])

  private val allVariables = HashMap[String, BrboType](
    "n" -> INT,
    "m" -> INT,
    "l" -> INT,
    "D100" -> INT,
    "C1" -> INT,
  )

  "Invariant inference for delta variable updates" should "succeed" in {
    InvariantInferenceUnitTest.deltaVariableUpdateTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val invariantInference = new InvariantInference(targetMethod)
        val solver = new Z3Solver
        val result = invariantInference.inferInvariant(
          solver,
          Locations(
            {
              case _: VariableTree => false // To ensure `D` is declared before each `assert(D==D)`
              case tree if TreeUtils.isCommand(tree) => true
              case _ => false
            },
            AFTER
          ),
          whichVariable = "D100",
          allVariables
        )
        logger.debug(result)
        solver.mkAssert(solver.mkExists(List(solver.mkIntVar("C1"), solver.mkIntVar("R"), solver.mkIntVar("i"), solver.mkIntVar("j")), result))

        val assertion1 = solver.mkGt(solver.mkIntVar("D100"), solver.mkIntVal(100))
        val z3result1 = solver.checkAssertionPushPop(assertion1, printUnsatCore = false)
        assert(StringCompare.ignoreWhitespaces(z3result1.toString, "false", s"${testCase.className}: D100>100"))

        val assertion2 = solver.mkLt(solver.mkIntVar("D100"), solver.mkIntVal(0))
        val z3result2 = solver.checkAssertionPushPop(assertion2, printUnsatCore = false)
        assert(StringCompare.ignoreWhitespaces(z3result2.toString, "false", s"${testCase.className}: D100<0"))

        val assertion3 = solver.mkLe(solver.mkIntVar("D100"), solver.mkIntVal(1))
        val z3result3 = solver.checkAssertionPushPop(assertion3, printUnsatCore = false)
        assert(StringCompare.ignoreWhitespaces(z3result3.toString, "true", s"${testCase.className}: D100<=1"))
    })
  }

  "Invariant inference for delta variable resets" should "succeed" in {
    InvariantInferenceUnitTest.deltaVariableResetTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val invariantInference = new InvariantInference(targetMethod)
        val solver = new Z3Solver
        val result = invariantInference.inferInvariant(
          solver,
          Locations(
            {
              case expressionStatementTree: ExpressionStatementTree =>
                GhostVariableUtils.extractReset(expressionStatementTree.getExpression, Delta) match {
                  case Some(_) => true
                  case None => false
                }
              case _ => false
            },
            BEFORE
          ),
          whichVariable = "D100",
          allVariables
        )
        logger.debug(result)
        solver.mkAssert(solver.mkExists(List(solver.mkIntVar("C1"), solver.mkIntVar("R"), solver.mkIntVar("i"), solver.mkIntVar("j")), result))

        val assertion1 = solver.mkGe(solver.mkIntVar("D100"), solver.mkIntVal(0))
        val z3result1 = solver.checkAssertionPushPop(assertion1, printUnsatCore = false)
        assert(StringCompare.ignoreWhitespaces(z3result1.toString, "true", s"${testCase.className}: D1>=0"))

        val assertion2 = solver.mkLt(solver.mkIntVar("D100"), solver.mkIntVal(0))
        val z3result2 = solver.checkAssertionPushPop(assertion2, printUnsatCore = false)
        assert(StringCompare.ignoreWhitespaces(z3result2.toString, "false", s"${testCase.className}: D1<0"))

        val assertion3 = solver.mkITE(
          solver.mkGe(solver.mkIntVar("n"), solver.mkIntVal(0)),
          solver.mkGt(solver.mkIntVar("D100"), solver.mkIntVar("n")),
          solver.mkFalse()
        )
        val z3result3 = solver.checkAssertionPushPop(assertion3, printUnsatCore = false)
        assert(StringCompare.ignoreWhitespaces(z3result3.toString, "false", s"${testCase.className}: n>=0 ? D1>n : false"))
    })
  }

  "Invariant inference for counter variable updates" should "succeed" in {
    InvariantInferenceUnitTest.counterVariableUpdateTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val lastTree = targetMethod.methodTree.getBody.getStatements.asScala.last
        val invariantInference = new InvariantInference(targetMethod)
        val solver = new Z3Solver
        val result = invariantInference.inferInvariant(
          solver,
          Locations(
            {
              tree: Tree => if (tree == lastTree) true else false
            },
            AFTER
          ),
          whichVariable = "C1",
          allVariables
        )
        logger.debug(result)
        solver.mkAssert(solver.mkExists(List(solver.mkIntVar("D100"), solver.mkIntVar("R"), solver.mkIntVar("i"), solver.mkIntVar("j")), result))

        val assertion1 = solver.mkLe(solver.mkIntVar("C1"), solver.mkIntVal(100))
        val z3result1 = solver.checkAssertionPushPop(assertion1, printUnsatCore = false)
        assert(StringCompare.ignoreWhitespaces(z3result1.toString, "true", s"${testCase.className}: C1<=100"))

        val assertion2 = solver.mkNot(
          solver.mkITE(
            solver.mkGe(solver.mkIntVar("n"), solver.mkIntVal(0)),
            solver.mkLe(solver.mkIntVar("C1"), solver.mkIntVar("n")),
            solver.mkTrue()
          )
        )
        val z3result2 = solver.checkAssertionPushPop(assertion2, printUnsatCore = false)
        assert(StringCompare.ignoreWhitespaces(z3result2.toString, "false", s"${testCase.className}: !(n>=0 ? C1<=n : true)"))

        val assertion3 = solver.mkLt(solver.mkIntVar("C1"), solver.mkIntVal(0))
        val z3result3 = solver.checkAssertionPushPop(assertion3, printUnsatCore = false)
        assert(StringCompare.ignoreWhitespaces(z3result3.toString, "false", s"${testCase.className}: C1<0"))

        val assertion4 = solver.mkGt(solver.mkIntVar("C1"), solver.mkIntVal(100))
        val z3result4 = solver.checkAssertionPushPop(assertion4, printUnsatCore = false)
        assert(StringCompare.ignoreWhitespaces(z3result4.toString, "true", s"${testCase.className}: C1>100"))
    })
  }
}

object InvariantInferenceUnitTest {
  val test01: String = // A loop with a nesting depth of 1
    """class Test01 {
      |  void f(int n)
      |  {
      |    int D100 = 0;
      |    int C1 = 0;
      |    int R = 0;
      |    int i = 0;
      |    while (i < n)
      |    {
      |      i++;
      |      C1 = C1 + 1;
      |      D100 = 0;
      |      D100 = D100 + 1;
      |      R = R + 1;
      |    }
      |  }
      |}""".stripMargin

  val test02: String = // A loop with a nesting depth of 2
    """class Test02 {
      |  void f(int n, int m, int l)
      |  {
      |    int R = 0;
      |    int C1 = 0;
      |    int D100 = 0;
      |    int i = 0;
      |    while (i < n) {
      |      while (i < n) {
      |        i++;
      |        R = R + 1;
      |        C1 = C1 + 1;
      |        D100 = 0;
      |        D100 = D100 + 1;
      |      }
      |    }
      |  }
      |}""".stripMargin

  private val dontCare = "dontCare"

  val deltaVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, dontCare),
      TestCaseJavaProgram("Test02", test02, dontCare),
    )
  }

  val deltaVariableResetTests: HashSet[TestCaseJavaProgram] = {
    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, dontCare),
      TestCaseJavaProgram("Test02", test02, dontCare),
    )
  }

  val counterVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, dontCare),
      TestCaseJavaProgram("Test02", test02, dontCare),
    )
  }
}