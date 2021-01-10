package brbo.boundinference

import brbo.TestCaseJavaProgram
import brbo.boundinference.BoundChecking.DeltaCounterPair
import brbo.common.Z3Solver
import com.microsoft.z3.AST
import com.sun.source.tree.MethodTree
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashSet

class BoundCheckingUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[BoundCheckingUnitTest])

  "Extracting bound expression" should "succeed" in {

  }

  "Bound checking" should "succeed" in {
    val boundExpressions: List[(Z3Solver, AST)] = {
      val solver1 = new Z3Solver
      val boundExpression1 = solver1.mkLe(
        solver1.mkIntVar("R"),
        solver1.mkIntVar("n")
      )
      val solver2 = new Z3Solver
      val boundExpression2 = solver2.mkLe(
        solver2.mkIntVar("R"),
        solver2.mkIntVal(1)
      )
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
            solver3.mkAdd(m, solver3.mkMul(n, m))
          ),
          solver3.mkTrue()
        )
      }
      val solver4 = new Z3Solver
      val boundExpression4 = solver4.mkLe(
        solver4.mkIntVar("R"),
        solver4.mkMul(solver4.mkIntVar("n"), solver4.mkIntVar("m")),
      )
      List((solver3, boundExpression3))
      // List((solver1, boundExpression1), (solver2, boundExpression2), (solver3, boundExpression3))
      // List((solver3, boundExpression3), (solver4, boundExpression4))
    }

    BoundCheckingUnitTest.boundCheckingTests.zip(boundExpressions).foreach({
      case (testCase, (solver, boundExpression)) =>
        val basicProcessor = BasicProcessor.run(testCase.className, testCase.inputProgram)
        basicProcessor.assumeOneClassOneMethod()

        val methodTree: MethodTree = basicProcessor.getMethods.head._1
        val cfg = basicProcessor.getMethods.head._2
        val className = basicProcessor.getEnclosingClass(methodTree).get.getSimpleName.toString

        val deltaCounterPairs = Set[DeltaCounterPair](DeltaCounterPair("D100", "C1"))
        val result = BoundChecking.checkBound(
          solver,
          className,
          methodTree,
          basicProcessor.getLineNumber,
          cfg,
          deltaCounterPairs,
          boundExpression
        )
        assert(result.toString == testCase.expectedOutput)
    })
  }
}

object BoundCheckingUnitTest {
  private val test01 =
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

  private val test02 =
    """class Test02 {
      |  void f(int n, int m, int l)
      |  {
      |    int R = 0;
      |    int C1 = 0;
      |    int D100 = 0;
      |    int i = 0;
      |    while (i < n) {
      |      int j = 0;
      |      C1++;
      |      D100 = 0;
      |      while (j < m) {
      |        j++;
      |        R++;
      |        D100++;
      |      }
      |      i++;
      |    }
      |  }
      |}""".stripMargin

  val deltaVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (R Int)
        |                    (i Int)
        |                    (|param0:12| Int)
        |                    (|R':81| Int)
        |                    (|C1':83| Int)
        |                    (|K:79| Int)
        |                    (|i':82| Int)
        |                    (|D100':80| Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:79|) (<= |K:79| 0)))
        |                               (= |D100':80| 0)))
        |                      (a!2 (or (not (and (<= 0 |K:79|) (<= |K:79| 0)))
        |                               (= |R':81| |K:79|)))
        |                      (a!3 (or (not (and (<= 0 |K:79|) (<= |K:79| 0)))
        |                               (= |C1':83| |K:79|)))
        |                      (a!4 (and (<= 1 |K:79|)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |R':81|) |C1':83|) 0)
        |                                (= (+ (- 0 |R':81|) |i':82|) 0)
        |                                (= (+ (- 0 1) |D100':80|) 0)
        |                                (<= 0 (+ (- 0 |R':81|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |R':81|)))))
        |                  (and a!1
        |                       (or (not (<= 1 |K:79|)) (= |D100':80| 1))
        |                       a!2
        |                       (or (not (<= 1 |K:79|)) (= |R':81| |K:79|))
        |                       (or (not (<= 0 |K:79|)) (= |i':82| |K:79|))
        |                       a!3
        |                       (or (not (<= 1 |K:79|)) (= |C1':83| |K:79|))
        |                       (or (not (<= 0 |K:79|)) (<= |D100':80| |K:79|))
        |                       (or (and (= |K:79| 0)
        |                                (= 0 |i':82|)
        |                                (= 0 |R':81|)
        |                                (= 0 |C1':83|)
        |                                (= 0 |D100':80|))
        |                           a!4)
        |                       (<= 0 |K:79|)
        |                       (<= 0 |R':81|)
        |                       (<= 0 |D100':80|)
        |                       (<= 0 |C1':83|)
        |                       (<= 0 |i':82|)
        |                       (= (+ (- 0 |i':82|) |R':81|) 0)
        |                       (= (+ (- 0 |i':82|) |C1':83|) 0)
        |                       (< |i':82| |param0:12|)
        |                       (= D100 1)
        |                       (= C1 (+ |C1':83| 1))
        |                       (= R |R':81|)
        |                       (= i (+ |i':82| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (R Int)
        |                    (|j':119| Int)
        |                    (i Int)
        |                    (|K:115| Int)
        |                    (|C1':117| Int)
        |                    (|param0:14| Int)
        |                    (|param1:17| Int)
        |                    (|D100':118| Int)
        |                    (|R':120| Int)
        |                    (j Int)
        |                    (|R':124| Int)
        |                    (|K:121| Int)
        |                    (|j':122| Int)
        |                    (|D100':123| Int)
        |                    (|i':116| Int)
        |                    (|j:3| Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:115|) (<= |K:115| 0)))
        |                               (= |C1':117| |K:115|)))
        |                      (a!2 (or (not (and (<= 0 |K:115|) (<= |K:115| 0)))
        |                               (= (+ |D100':118| (- 0 |j':119|)) (- 0 |j:3|))))
        |                      (a!3 (or (not (<= 1 |K:115|))
        |                               (= (+ |D100':118| (- 0 |j':119|)) 0)))
        |                      (a!4 (or (not (<= 0 |K:115|))
        |                               (<= (+ (- 0 |R':120|) |D100':118|) 0)))
        |                      (a!5 (or (not (<= 0 |K:115|))
        |                               (<= (- 0 |R':120|) (- 0 (* |param1:17| |K:115|)))))
        |                      (a!6 (and (<= 1 |K:115|)
        |                                (<= 0 (+ (- 0 1) |param0:14|))
        |                                (= (+ (- 0 |j':119|) |D100':118|) 0)
        |                                (= (+ (- 0 |i':116|) |C1':117|) 0)
        |                                (<= 0 (+ (- 0 |j':119|) |R':120|))
        |                                (<= 0 (+ (- 0 |i':116|) |param0:14|))
        |                                (<= 0 (+ (- 0 1) |i':116|))
        |                                (<= 0 (+ |j':119| (- 0 |param1:17|)))
        |                                (<= 0 |j':119|)))
        |                      (a!7 (or (not (and (<= 0 |K:121|) (<= |K:121| 0)))
        |                               (= |D100':123| |K:121|)))
        |                      (a!8 (and (<= 1 |K:121|)
        |                                (<= 0 (+ (- 0 1) |param1:17|))
        |                                (<= 0 |R':120|)
        |                                (= (+ (- 0 |D100':123|) |j':122|) 0)
        |                                (<= 0 (+ (- 0 |D100':123|) |param1:17|))
        |                                (<= 0 (+ (- 0 1) |R':124|))
        |                                (<= 0 (+ (- 0 1) |D100':123|)))))
        |                  (and (or (not (<= 0 |K:115|)) (= |i':116| |K:115|))
        |                       a!1
        |                       (or (not (<= 1 |K:115|)) (= |C1':117| |K:115|))
        |                       a!2
        |                       a!3
        |                       a!4
        |                       a!5
        |                       (or (not (<= 0 |K:115|)) (<= (- 0 |R':120|) 0))
        |                       (or (and (= |K:115| 0)
        |                                (= |j:3| |j':119|)
        |                                (= 0 |i':116|)
        |                                (= 0 |D100':118|)
        |                                (= 0 |C1':117|)
        |                                (= 0 |R':120|))
        |                           a!6)
        |                       (<= 0 |K:115|)
        |                       (<= 0 |D100':118|)
        |                       (<= 0 |R':120|)
        |                       (<= 0 |C1':117|)
        |                       (<= 0 |i':116|)
        |                       (= (+ (- 0 |i':116|) |C1':117|) 0)
        |                       (< |i':116| |param0:14|)
        |                       (or (not (<= 0 |K:121|)) (= |j':122| |K:121|))
        |                       a!7
        |                       (or (not (<= 1 |K:121|)) (= |D100':123| |K:121|))
        |                       (or (not (<= 0 |K:121|))
        |                           (= |R':124| (+ |R':120| |K:121|)))
        |                       (or (and (= |K:121| 0)
        |                                (= 0 |j':122|)
        |                                (= 0 |D100':123|)
        |                                (= |R':120| |R':124|))
        |                           a!8)
        |                       (<= 0 |K:121|)
        |                       (<= 0 |D100':123|)
        |                       (<= 0 |R':124|)
        |                       (<= 0 |j':122|)
        |                       (= (+ (- 0 |j':122|) |D100':123|) 0)
        |                       (< |j':122| |param1:17|)
        |                       (= R (+ |R':124| 1))
        |                       (= C1 (+ |C1':117| 1))
        |                       (= D100 (+ |D100':123| 1))
        |                       (= i |i':116|)
        |                       (= j (+ |j':122| 1))
        |                       (= n |param0:14|)
        |                       (= m |param1:17|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01Expected),
      TestCaseJavaProgram("Test02", test02, test02Expected),
    )
  }

  val deltaVariableResetTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (R Int)
        |                    (i Int)
        |                    (|param0:12| Int)
        |                    (|D100':81| Int)
        |                    (|R':82| Int)
        |                    (|C1':84| Int)
        |                    (|K:80| Int)
        |                    (|i':83| Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:80|) (<= |K:80| 0)))
        |                               (= |D100':81| 0)))
        |                      (a!2 (or (not (and (<= 0 |K:80|) (<= |K:80| 0)))
        |                               (= |R':82| |K:80|)))
        |                      (a!3 (or (not (and (<= 0 |K:80|) (<= |K:80| 0)))
        |                               (= |C1':84| |K:80|)))
        |                      (a!4 (and (<= 1 |K:80|)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |R':82|) |C1':84|) 0)
        |                                (= (+ (- 0 |R':82|) |i':83|) 0)
        |                                (= (+ (- 0 1) |D100':81|) 0)
        |                                (<= 0 (+ (- 0 |R':82|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |R':82|)))))
        |                  (and a!1
        |                       (or (not (<= 1 |K:80|)) (= |D100':81| 1))
        |                       a!2
        |                       (or (not (<= 1 |K:80|)) (= |R':82| |K:80|))
        |                       (or (not (<= 0 |K:80|)) (= |i':83| |K:80|))
        |                       a!3
        |                       (or (not (<= 1 |K:80|)) (= |C1':84| |K:80|))
        |                       (or (not (<= 0 |K:80|)) (<= |D100':81| |K:80|))
        |                       (or (and (= |K:80| 0)
        |                                (= 0 |i':83|)
        |                                (= 0 |R':82|)
        |                                (= 0 |C1':84|)
        |                                (= 0 |D100':81|))
        |                           a!4)
        |                       (<= 0 |K:80|)
        |                       (<= 0 |R':82|)
        |                       (<= 0 |D100':81|)
        |                       (<= 0 |C1':84|)
        |                       (<= 0 |i':83|)
        |                       (= (+ (- 0 |i':83|) |R':82|) 0)
        |                       (= (+ (- 0 |i':83|) |C1':84|) 0)
        |                       (< |i':83| |param0:12|)
        |                       (= D100 |D100':81|)
        |                       (= C1 (+ |C1':84| 1))
        |                       (= R |R':82|)
        |                       (= i (+ |i':83| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (R Int)
        |                    (|j':119| Int)
        |                    (i Int)
        |                    (|K:115| Int)
        |                    (|C1':117| Int)
        |                    (|param0:14| Int)
        |                    (|param1:17| Int)
        |                    (|D100':118| Int)
        |                    (|R':120| Int)
        |                    (j Int)
        |                    (|i':116| Int)
        |                    (|j:3| Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:115|) (<= |K:115| 0)))
        |                               (= |C1':117| |K:115|)))
        |                      (a!2 (or (not (and (<= 0 |K:115|) (<= |K:115| 0)))
        |                               (= (+ |D100':118| (- 0 |j':119|)) (- 0 |j:3|))))
        |                      (a!3 (or (not (<= 1 |K:115|))
        |                               (= (+ |D100':118| (- 0 |j':119|)) 0)))
        |                      (a!4 (or (not (<= 0 |K:115|))
        |                               (<= (+ (- 0 |R':120|) |D100':118|) 0)))
        |                      (a!5 (or (not (<= 0 |K:115|))
        |                               (<= (- 0 |R':120|) (- 0 (* |param1:17| |K:115|)))))
        |                      (a!6 (and (<= 1 |K:115|)
        |                                (<= 0 (+ (- 0 1) |param0:14|))
        |                                (= (+ (- 0 |j':119|) |D100':118|) 0)
        |                                (= (+ (- 0 |i':116|) |C1':117|) 0)
        |                                (<= 0 (+ (- 0 |j':119|) |R':120|))
        |                                (<= 0 (+ (- 0 |i':116|) |param0:14|))
        |                                (<= 0 (+ (- 0 1) |i':116|))
        |                                (<= 0 (+ |j':119| (- 0 |param1:17|)))
        |                                (<= 0 |j':119|))))
        |                  (and (or (not (<= 0 |K:115|)) (= |i':116| |K:115|))
        |                       a!1
        |                       (or (not (<= 1 |K:115|)) (= |C1':117| |K:115|))
        |                       a!2
        |                       a!3
        |                       a!4
        |                       a!5
        |                       (or (not (<= 0 |K:115|)) (<= (- 0 |R':120|) 0))
        |                       (or (and (= |K:115| 0)
        |                                (= |j:3| |j':119|)
        |                                (= 0 |i':116|)
        |                                (= 0 |D100':118|)
        |                                (= 0 |C1':117|)
        |                                (= 0 |R':120|))
        |                           a!6)
        |                       (<= 0 |K:115|)
        |                       (<= 0 |D100':118|)
        |                       (<= 0 |R':120|)
        |                       (<= 0 |C1':117|)
        |                       (<= 0 |i':116|)
        |                       (= (+ (- 0 |i':116|) |C1':117|) 0)
        |                       (< |i':116| |param0:14|)
        |                       (= R |R':120|)
        |                       (= C1 (+ |C1':117| 1))
        |                       (= D100 |D100':118|)
        |                       (= i |i':116|)
        |                       (= j 0)
        |                       (= n |param0:14|)
        |                       (= m |param1:17|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01Expected),
      TestCaseJavaProgram("Test02", test02, test02Expected),
    )
  }

  val counterVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected =
      """(let ((a!1 (exists ((D100 Int)
        |                    (R Int)
        |                    (i Int)
        |                    (|param0:12| Int)
        |                    (|D100':81| Int)
        |                    (|R':82| Int)
        |                    (|C1':84| Int)
        |                    (|K:80| Int)
        |                    (|i':83| Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:80|) (<= |K:80| 0)))
        |                               (= |D100':81| 0)))
        |                      (a!2 (or (not (and (<= 0 |K:80|) (<= |K:80| 0)))
        |                               (= |R':82| |K:80|)))
        |                      (a!3 (or (not (and (<= 0 |K:80|) (<= |K:80| 0)))
        |                               (= |C1':84| |K:80|)))
        |                      (a!4 (and (<= 1 |K:80|)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |R':82|) |C1':84|) 0)
        |                                (= (+ (- 0 |R':82|) |i':83|) 0)
        |                                (= (+ (- 0 1) |D100':81|) 0)
        |                                (<= 0 (+ (- 0 |R':82|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |R':82|)))))
        |                  (and a!1
        |                       (or (not (<= 1 |K:80|)) (= |D100':81| 1))
        |                       a!2
        |                       (or (not (<= 1 |K:80|)) (= |R':82| |K:80|))
        |                       (or (not (<= 0 |K:80|)) (= |i':83| |K:80|))
        |                       a!3
        |                       (or (not (<= 1 |K:80|)) (= |C1':84| |K:80|))
        |                       (or (not (<= 0 |K:80|)) (<= |D100':81| |K:80|))
        |                       (or (and (= |K:80| 0)
        |                                (= 0 |i':83|)
        |                                (= 0 |R':82|)
        |                                (= 0 |C1':84|)
        |                                (= 0 |D100':81|))
        |                           a!4)
        |                       (<= 0 |K:80|)
        |                       (<= 0 |R':82|)
        |                       (<= 0 |D100':81|)
        |                       (<= 0 |C1':84|)
        |                       (<= 0 |i':83|)
        |                       (= (+ (- 0 |i':83|) |R':82|) 0)
        |                       (= (+ (- 0 |i':83|) |C1':84|) 0)
        |                       (< |i':83| |param0:12|)
        |                       (= D100 |D100':81|)
        |                       (= C1 (+ |C1':84| 1))
        |                       (= R |R':82|)
        |                       (= i (+ |i':83| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((D100 Int)
        |                    (R Int)
        |                    (|j':119| Int)
        |                    (i Int)
        |                    (|K:115| Int)
        |                    (|C1':117| Int)
        |                    (|param0:14| Int)
        |                    (|param1:17| Int)
        |                    (|D100':118| Int)
        |                    (|R':120| Int)
        |                    (j Int)
        |                    (|i':116| Int)
        |                    (|j:3| Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:115|) (<= |K:115| 0)))
        |                               (= |C1':117| |K:115|)))
        |                      (a!2 (or (not (and (<= 0 |K:115|) (<= |K:115| 0)))
        |                               (= (+ |D100':118| (- 0 |j':119|)) (- 0 |j:3|))))
        |                      (a!3 (or (not (<= 1 |K:115|))
        |                               (= (+ |D100':118| (- 0 |j':119|)) 0)))
        |                      (a!4 (or (not (<= 0 |K:115|))
        |                               (<= (+ (- 0 |R':120|) |D100':118|) 0)))
        |                      (a!5 (or (not (<= 0 |K:115|))
        |                               (<= (- 0 |R':120|) (- 0 (* |param1:17| |K:115|)))))
        |                      (a!6 (and (<= 1 |K:115|)
        |                                (<= 0 (+ (- 0 1) |param0:14|))
        |                                (= (+ (- 0 |j':119|) |D100':118|) 0)
        |                                (= (+ (- 0 |i':116|) |C1':117|) 0)
        |                                (<= 0 (+ (- 0 |j':119|) |R':120|))
        |                                (<= 0 (+ (- 0 |i':116|) |param0:14|))
        |                                (<= 0 (+ (- 0 1) |i':116|))
        |                                (<= 0 (+ |j':119| (- 0 |param1:17|)))
        |                                (<= 0 |j':119|))))
        |                  (and (or (not (<= 0 |K:115|)) (= |i':116| |K:115|))
        |                       a!1
        |                       (or (not (<= 1 |K:115|)) (= |C1':117| |K:115|))
        |                       a!2
        |                       a!3
        |                       a!4
        |                       a!5
        |                       (or (not (<= 0 |K:115|)) (<= (- 0 |R':120|) 0))
        |                       (or (and (= |K:115| 0)
        |                                (= |j:3| |j':119|)
        |                                (= 0 |i':116|)
        |                                (= 0 |D100':118|)
        |                                (= 0 |C1':117|)
        |                                (= 0 |R':120|))
        |                           a!6)
        |                       (<= 0 |K:115|)
        |                       (<= 0 |D100':118|)
        |                       (<= 0 |R':120|)
        |                       (<= 0 |C1':117|)
        |                       (<= 0 |i':116|)
        |                       (= (+ (- 0 |i':116|) |C1':117|) 0)
        |                       (< |i':116| |param0:14|)
        |                       (= R |R':120|)
        |                       (= C1 (+ |C1':117| 1))
        |                       (= D100 |D100':118|)
        |                       (= i |i':116|)
        |                       (= j 0)
        |                       (= n |param0:14|)
        |                       (= m |param1:17|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01Expected),
      TestCaseJavaProgram("Test02", test02, test02Expected),
    )
  }

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

  private val doWhileTest =
    """class DoWhileTest {
      |  void f(int n) {
      |    int i = 0;
      |    do {
      |      i++;
      |    } while (i < n);
      |  }
      |}""".stripMargin

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

    val doWhileTestExpected =
      """List(({
        |    int i = 0;
        |    do {
        |        i++;
        |    }     while (i < n);
        |},C0), (int i = 0,C1), (do {
        |    i++;
        |} while (i < n);,C2), ({
        |    i++;
        |},C3), (i++;,C4))""".stripMargin

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
      TestCaseJavaProgram("DoWhileTest", doWhileTest, doWhileTestExpected),
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

    val doWhileTestExpected =
      """(and (>= (+ C1 1) C0)
        |     (>= C0 C1)
        |     (>= (+ C2 1) C1)
        |     (>= C1 C2)
        |     true
        |     (>= C3 C2)
        |     (>= (+ C4 1) C3)
        |     (>= C3 C4)
        |     true)""".stripMargin

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
      TestCaseJavaProgram("DoWhileTest", doWhileTest, doWhileTestExpected),
      TestCaseJavaProgram("ForLoopTest", forLoopTest, forLoopTestExpected),
      TestCaseJavaProgram("IfTest", ifTest, ifTestExpected),
      TestCaseJavaProgram("WhileLoopTest", whileLoopTest, whileLoopExpected),
    )
  }

  val boundCheckingTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected = "true"
    val test01Expected2 = "false"
    val test02Expected = "true"
    val test02Expected2 = "false"

    HashSet[TestCaseJavaProgram](
      // TestCaseJavaProgram("Test01", test01, test01Expected),
      // TestCaseJavaProgram("Test01", test01, test01Expected2),
      TestCaseJavaProgram("Test02", test02, test02Expected),
      // TestCaseJavaProgram("Test02", test02, test02Expected2),
    )
  }
}