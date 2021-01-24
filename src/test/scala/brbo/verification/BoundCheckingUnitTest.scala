package brbo.verification

import brbo.common.{CommandLineArguments, Z3Solver}
import brbo.verification.AmortizationMode.UNKNOWN
import brbo.verification.Decomposition.{DecompositionResult, DeltaCounterPair}
import brbo.{StringCompare, TestCaseJavaProgram}
import com.microsoft.z3.AST
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashSet

class BoundCheckingUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[BoundCheckingUnitTest])

  "Extracting bound expression" should "succeed" in {

  }

  "Bound checking" should "succeed" in {
    BoundCheckingUnitTest.boundCheckingTests.foreach({
      case (testCase, solver, boundExpression) =>
        val deltaCounterPairs = Set[DeltaCounterPair](DeltaCounterPair("D100", "C1"))
        val inputMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decompositionResult = DecompositionResult(testCase.inputProgram, deltaCounterPairs, UNKNOWN, inputMethod)
        val result = BoundChecking.checkBound(
          solver,
          decompositionResult,
          boundExpression,
          CommandLineArguments(UNKNOWN, debugMode = false, "", skipSanityCheck = false, testCase.expectedOutput == "true")
        )
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, s"Test case ${testCase.className} failed"))
    })
  }
}

object BoundCheckingUnitTest {
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

  val test03: String = // The resource is updated at 2 places
    """class Test03 {
      |  void f(int n)
      |  {
      |    int R = 0;
      |    int C1 = 0;
      |    int D100 = 0;
      |    int i = 0;
      |    C1 = C1 + 1;
      |    D100 = 0;
      |    D100 = D100 + 1;
      |    R = R + 1;
      |    while (i < n) {
      |      R = R + 1;
      |      D100 = D100 + 1;
      |      i++;
      |    }
      |  }
      |}""".stripMargin

  val test04: String = // The resource increases and decreases
    """class Test04 {
      |  void f(int n)
      |  {
      |    int R = 0;
      |    int C1 = 0;
      |    int D100 = 0;
      |    int i = 0;
      |    C1 = C1 + 1;
      |    D100 = 0;
      |    R = R + n;
      |    D100 = D100 + n;
      |    R = R - n;
      |    D100 = D100 - n;
      |    R = R + n;
      |    D100 = D100 + n;
      |  }
      |}""".stripMargin

  val test05: String = // A loop with a nesting depth of 2
    """class Test05 {
      |  void f(int n, int m, int l)
      |  {
      |    int R = 0;
      |    int C1 = 0;
      |    int D100 = 0;
      |    int x = 0;
      |    while (x < n) {
      |      while (x < n) {
      |        R = R + 1;
      |        C1 = C1 + 1;
      |        D100 = 0;
      |        D100 = D100 + 1;
      |        x++;
      |      }
      |    }
      |  }
      |}""".stripMargin

  private val blockTest =
    """class BlockTest {
      |  void f(int n) {
      |    int i = 0;
      |    i++;
      |    int j = 0;
      |    j = i + 1;
      |    ;
      |    assert (j >= 0);
      |    return;
      |  }
      |}""".stripMargin

  /*private val doWhileTest =
    """class DoWhileTest {
      |  void f(int n) {
      |    int i = 0;
      |    do {
      |      i++;
      |    } while (i < n);
      |  }
      |}""".stripMargin*/

  private val forLoopTest =
    """class ForLoopTest {
      |  void f(int n) {
      |    for (int i = 0; i < n; i++) {
      |      i += 2;
      |    }
      |  }
      |}""".stripMargin

  private val ifTest =
    """class IfTest {
      |  void f(int n) {
      |    int i = 0;
      |    if (n > 0) {
      |      i += 1;
      |    }
      |    else {
      |      i += -1;
      |    }
      |  }
      |}""".stripMargin

  private val whileLoopTest =
    """class WhileLoopTest {
      |  void f(int n) {
      |    int i = 0;
      |    while (i < n) {
      |      i++;
      |    }
      |  }
      |}""".stripMargin

  val counterGenerationTests: HashSet[TestCaseJavaProgram] = {
    val blockTestExpected =
      """List(({
        |    int i = 0;
        |    i++;
        |    int j = 0;
        |    j = i + 1;
        |    ;
        |    assert (j >= 0);
        |    return;
        |},C0), (int i = 0,C1), (i++;,C2), (int j = 0,C3), (j = i + 1;,C4), (;,C5), (assert (j >= 0);,C6), (return;,C7))""".stripMargin

    /*val doWhileTestExpected =
      """List(({
        |    int i = 0;
        |    do {
        |        i++;
        |    }     while (i < n);
        |},C0), (int i = 0,C1), (do {
        |    i++;
        |} while (i < n);,C2), ({
        |    i++;
        |},C3), (i++;,C4))""".stripMargin*/

    val forLoopTestExpected =
      """List(({
        |    for (int i = 0; i < n; i++) {
        |        i += 2;
        |    }
        |},C0), (for (int i = 0; i < n; i++) {
        |    i += 2;
        |},C1), (int i = 0,C2), ({
        |    i += 2;
        |},C3), (i += 2;,C4), (i++;,C5))""".stripMargin

    val ifTestExpected =
      """List(({
        |    int i = 0;
        |    if (n > 0) {
        |        i += 1;
        |    } else {
        |        i += -1;
        |    }
        |},C0), (int i = 0,C1), (if (n > 0) {
        |    i += 1;
        |} else {
        |    i += -1;
        |},C2), ({
        |    i += 1;
        |},C3), (i += 1;,C4), ({
        |    i += -1;
        |},C5), (i += -1;,C6))""".stripMargin

    val whileLoopExpected =
      """List(({
        |    int i = 0;
        |    while (i < n) {
        |        i++;
        |    }
        |},C0), (int i = 0,C1), (while (i < n) {
        |    i++;
        |},C2), ({
        |    i++;
        |},C3), (i++;,C4))""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("BlockTest", blockTest, blockTestExpected),
      // TestCaseJavaProgram("DoWhileTest", doWhileTest, doWhileTestExpected),
      TestCaseJavaProgram("ForLoopTest", forLoopTest, forLoopTestExpected),
      TestCaseJavaProgram("IfTest", ifTest, ifTestExpected),
      TestCaseJavaProgram("WhileLoopTest", whileLoopTest, whileLoopExpected),
    )
  }

  val counterAxiomsTests: HashSet[TestCaseJavaProgram] = {
    val blockTestExpected =
      """(and (>= (+ C1 1) C0)
        |     (>= C0 C1)
        |     (>= (+ C2 1) C1)
        |     (>= C1 C2)
        |     (>= (+ C3 1) C2)
        |     (>= C2 C3)
        |     (>= (+ C4 1) C3)
        |     (>= C3 C4)
        |     (>= (+ C5 1) C4)
        |     (>= C4 C5)
        |     (>= (+ C6 1) C5)
        |     (>= C5 C6)
        |     (>= (+ C7 1) C6)
        |     (>= C6 C7)
        |     true
        |     true
        |     true
        |     true
        |     true
        |     true
        |     true)""".stripMargin

    /*val doWhileTestExpected =
      """(and (>= (+ C1 1) C0)
        |     (>= C0 C1)
        |     (>= (+ C2 1) C1)
        |     (>= C1 C2)
        |     true
        |     (>= C3 C2)
        |     (>= (+ C4 1) C3)
        |     (>= C3 C4)
        |     true)""".stripMargin*/

    val forLoopTestExpected =
      """(and (>= (+ C1 1) C0)
        |     (>= C0 C1)
        |     (>= (+ C2 1) C1)
        |     (>= C1 C2)
        |     true
        |     (>= (+ C5 1) C3)
        |     (>= C3 C5)
        |     true
        |     (>= (+ C4 1) C3)
        |     (>= C3 C4)
        |     true)""".stripMargin

    val ifTestExpected =
      """(and (>= (+ C1 1) C0)
        |     (>= C0 C1)
        |     (>= (+ C2 1) C1)
        |     (>= C1 C2)
        |     true
        |     (>= (+ C3 C5 1) C2)
        |     (>= C2 (+ C3 C5))
        |     (>= (+ C4 1) C3)
        |     (>= C3 C4)
        |     (and true)
        |     (>= (+ C6 1) C5)
        |     (>= C5 C6)
        |     (and true))""".stripMargin

    val whileLoopExpected =
      """(and (>= (+ C1 1) C0)
        |     (>= C0 C1)
        |     (>= (+ C2 1) C1)
        |     (>= C1 C2)
        |     true
        |     (>= (+ C4 1) C3)
        |     (>= C3 C4)
        |     true)""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("BlockTest", blockTest, blockTestExpected),
      // TestCaseJavaProgram("DoWhileTest", doWhileTest, doWhileTestExpected),
      TestCaseJavaProgram("ForLoopTest", forLoopTest, forLoopTestExpected),
      TestCaseJavaProgram("IfTest", ifTest, ifTestExpected),
      TestCaseJavaProgram("WhileLoopTest", whileLoopTest, whileLoopExpected),
    )
  }

  val boundCheckingTests: List[(TestCaseJavaProgram, Z3Solver, AST)] = {
    val TRUE = "true"
    val FALSE = "false"

    val solver1 = new Z3Solver
    val boundExpression1 = {
      val R = solver1.mkIntVar("R")
      val n = solver1.mkIntVar("n")
      solver1.mkITE(
        solver1.mkGe(n, solver1.mkIntVal(0)),
        solver1.mkLe(R, n),
        solver1.mkTrue()
      )
    }

    val solver2 = new Z3Solver
    val boundExpression2 = {
      val R = solver2.mkIntVar("R")
      val n = solver2.mkIntVar("n")
      solver2.mkITE(
        solver2.mkGe(n, solver2.mkIntVal(0)),
        solver2.mkLe(R, solver2.mkSub(n, solver2.mkIntVal(10))),
        solver2.mkFalse()
      )
    }

    val solver3 = new Z3Solver
    val boundExpression3 = {
      val n = solver3.mkIntVar("n")
      val m = solver3.mkIntVar("m")
      solver3.mkITE(
        solver3.mkAnd(
          solver3.mkGe(n, solver3.mkIntVal(0)),
          solver3.mkGe(m, solver3.mkIntVal(0))
        ),
        solver3.mkLe(
          solver3.mkIntVar("R"),
          solver3.mkMul(n, m)
        ),
        solver3.mkTrue()
      )
    }

    val solver4 = new Z3Solver
    val boundExpression4 = {
      val n = solver4.mkIntVar("n")
      val m = solver4.mkIntVar("m")
      solver4.mkITE(
        solver4.mkAnd(
          solver4.mkGe(n, solver4.mkIntVal(0)),
          solver4.mkGe(m, solver4.mkIntVal(0))
        ),
        solver4.mkLe(
          solver4.mkIntVar("R"),
          n
        ),
        solver4.mkFalse()
      )
    }

    val solver5 = new Z3Solver
    val boundExpression5 = {
      val n = solver5.mkIntVar("n")
      solver5.mkITE(
        solver5.mkGe(n, solver5.mkIntVal(0)),
        solver5.mkLe(
          solver5.mkIntVar("R"),
          solver5.mkAdd(n, solver5.mkIntVal(1))
        ),
        solver5.mkTrue()
      )
    }

    val solver6 = new Z3Solver
    val boundExpression6 = {
      val n = solver6.mkIntVar("n")
      solver6.mkITE(
        solver6.mkGe(n, solver6.mkIntVal(0)),
        solver6.mkLe(
          solver6.mkIntVar("R"),
          n
        ),
        solver6.mkFalse()
      )
    }

    val solver7 = new Z3Solver
    val boundExpression7 = {
      val n = solver7.mkIntVar("n")
      solver7.mkITE(
        solver7.mkGe(n, solver7.mkIntVal(0)),
        solver7.mkLe(
          solver7.mkIntVar("R"),
          n
        ),
        solver7.mkTrue()
      )
    }

    val solver8 = new Z3Solver
    val boundExpression8 = {
      val n = solver8.mkIntVar("n")
      solver8.mkITE(
        solver8.mkGe(n, solver8.mkIntVal(0)),
        solver8.mkLe(
          solver8.mkIntVar("R"),
          solver8.mkIntVal(1)
        ),
        solver8.mkFalse()
      )
    }

    val solver9 = new Z3Solver
    val boundExpression9 = {
      val n = solver9.mkIntVar("n")
      solver9.mkITE(
        solver9.mkGe(n, solver9.mkIntVal(0)),
        solver9.mkLe(
          solver9.mkIntVar("R"),
          solver9.mkIntVar("n")
        ),
        solver9.mkTrue()
      )
    }

    List[(TestCaseJavaProgram, Z3Solver, AST)](
      (TestCaseJavaProgram("Test01", test01, TRUE), solver1, boundExpression1),
      (TestCaseJavaProgram("Test01", test01, FALSE), solver2, boundExpression2),
      (TestCaseJavaProgram("Test02", test02, TRUE), solver3, boundExpression3),
      (TestCaseJavaProgram("Test02", test02, FALSE), solver4, boundExpression4),
      (TestCaseJavaProgram("Test03", test03, TRUE), solver5, boundExpression5),
      (TestCaseJavaProgram("Test03", test03, FALSE), solver6, boundExpression6),
      (TestCaseJavaProgram("Test04", test04, TRUE), solver7, boundExpression7),
      (TestCaseJavaProgram("Test04", test04, FALSE), solver8, boundExpression8),
      (TestCaseJavaProgram("Test05", test05, TRUE), solver9, boundExpression9),
    )
  }
}