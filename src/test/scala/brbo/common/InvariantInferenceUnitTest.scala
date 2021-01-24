package brbo.common

import brbo.common.BeforeOrAfterOrThis.{AFTER, BEFORE}
import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta}
import brbo.common.TypeUtils.BrboType.{BrboType, INT}
import brbo.verification.{BasicProcessor, BoundCheckingUnitTest}
import brbo.{StringCompare, TestCaseJavaProgram}
import com.sun.source.tree.ExpressionStatementTree
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

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
              case expressionStatementTree: ExpressionStatementTree =>
                GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Delta) match {
                  case Some(_) => true
                  case None => false
                }
              case _ => false
            },
            AFTER
          ),
          whichVariable = "D100",
          allVariables
        )
        logger.debug(result)
        val assertion = solver.mkNot(solver.mkLe(solver.mkIntVar("D100"), solver.mkIntVal(1)))
        solver.mkAssert(solver.mkExists(List(solver.mkIntVar("C1"), solver.mkIntVar("R"), solver.mkIntVar("i"), solver.mkIntVar("j")), result))
        solver.mkAssert(assertion)
        val z3result = solver.checkSAT(false)
        assert(StringCompare.ignoreWhitespaces(z3result.toString, testCase.expectedOutput, testCase.className))
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
        val assertion = solver.mkNot(solver.mkLe(solver.mkIntVar("D100"), solver.mkIntVal(0)))
        solver.mkAssert(solver.mkExists(List(solver.mkIntVar("C1"), solver.mkIntVar("R"), solver.mkIntVar("i"), solver.mkIntVar("j")), result))
        solver.mkAssert(assertion)
        val z3result = solver.checkSAT(false)
        assert(StringCompare.ignoreWhitespaces(z3result.toString, testCase.expectedOutput, testCase.className))
    })
  }

  "Invariant inference for counter variable updates" should "succeed" in {
    InvariantInferenceUnitTest.counterVariableUpdateTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val invariantInference = new InvariantInference(targetMethod)
        val solver = new Z3Solver
        val result = invariantInference.inferInvariant(
          solver,
          Locations(
            {
              case expressionStatementTree: ExpressionStatementTree =>
                GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Counter) match {
                  case Some(_) => true
                  case None => false
                }
              case _ => false
            },
            AFTER
          ),
          whichVariable = "C1",
          HashMap[String, BrboType](
            "n" -> INT,
            "m" -> INT,
            "l" -> INT,
            "C1" -> INT
          )
        )
        logger.debug(result)
        val assertion = solver.mkNot(solver.mkLe(solver.mkIntVar("C1"), solver.mkIntVar("n")))
        solver.mkAssert(solver.mkExists(List(solver.mkIntVar("D100"), solver.mkIntVar("R"), solver.mkIntVar("i"), solver.mkIntVar("j")), result))
        solver.mkAssert(assertion)
        val z3result = solver.checkSAT(false)
        assert(StringCompare.ignoreWhitespaces(z3result.toString, testCase.expectedOutput, testCase.className))
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
      |      int j = 0;
      |      C1 = C1 + 1;
      |      D100 = 0;
      |      while (j < m) {
      |        j++;
      |        R = R + 1;
      |        D100 = D100 + 1;
      |      }
      |      i++;
      |    }
      |  }
      |}""".stripMargin

  val deltaVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, "false"),
      TestCaseJavaProgram("Test02", test02, "false"),
    )
  }

  val deltaVariableResetTests: HashSet[TestCaseJavaProgram] = {
    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, "false"),
      TestCaseJavaProgram("Test02", test02, "false"),
    )
  }

  val counterVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, "false"),
      TestCaseJavaProgram("Test02", test02, "false"),
    )
  }
}