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
    BoundCheckingUnitTest.boundCheckingTests.foreach({
      case (testCase, solver, boundExpression) =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)

        val deltaCounterPairs = Set[DeltaCounterPair](DeltaCounterPair("D100", "C1"))
        val result = BoundChecking.checkBound(
          solver,
          targetMethod.className,
          targetMethod.methodTree,
          targetMethod.getLineNumber,
          targetMethod.cfg,
          deltaCounterPairs,
          boundExpression,
          printModelIfFail = false
        )
        assert(result.toString == testCase.expectedOutput, s"Test case ${testCase.className} failed")
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

  val test03: String = // The resource is updated at 2 places
    """class Test03 {
      |  void f(int n)
      |  {
      |    int R = 0;
      |    int C1 = 0;
      |    int D100 = 0;
      |    int i = 0;
      |    C1++;
      |    D100 = 0;
      |    D100++;
      |    R++;
      |    while (i < n) {
      |      R++;
      |      D100++;
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
      |    C1++;
      |    D100 = 0;
      |    R = R + n;
      |    D100 = D100 + n;
      |    R = R - n;
      |    D100 = D100 - n;
      |    R = R + n;
      |    D100 = D100 + n;
      |  }
      |}""".stripMargin

  val deltaVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (j Int)
        |                    (i Int)
        |                    (R Int)
        |                    (|R':96| Int)
        |                    (|K:90| Int)
        |                    (|D100':98| Int)
        |                    (|param0:12| Int)
        |                    (|C1':97| Int)
        |                    (|mid_R:92| Int)
        |                    (|mid_i:89| Int)
        |                    (|K:99| Int)
        |                    (|mid_C1:93| Int)
        |                    (|i':95| Int)
        |                    (|mid_D100:91| Int)
        |                    (|K:94| Int))
        |             (! (let ((a!1 (and (<= 1 |K:90|)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |mid_R:92|) |mid_C1:93|) 0)
        |                                (= (+ (- 0 |mid_R:92|) |mid_i:89|) 0)
        |                                (= (+ (- 0 1) |mid_D100:91|) 0)
        |                                (<= 0 (+ (- 0 |mid_R:92|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |mid_R:92|)))))
        |                  (and (= |mid_i:89| |K:90|)
        |                       (= |mid_D100:91| (ite (= |K:90| 0) 0 1))
        |                       (= (+ |mid_i:89| (- 0 |mid_R:92|)) 0)
        |                       (= (+ |mid_C1:93| (- 0 |mid_R:92|)) 0)
        |                       (<= |mid_D100:91| |K:90|)
        |                       (= |K:90| |K:90|)
        |                       (or (and (= |K:90| 0)
        |                                (= 0 |mid_i:89|)
        |                                (= 0 |mid_R:92|)
        |                                (= 0 |mid_C1:93|)
        |                                (= 0 |mid_D100:91|))
        |                           a!1)
        |                       (= |K:94| |K:94|)
        |                       (= |K:94| 0)
        |                       (= |mid_i:89| |i':95|)
        |                       (= |mid_R:92| |R':96|)
        |                       (= |mid_C1:93| |C1':97|)
        |                       (= |mid_D100:91| |D100':98|)
        |                       (= 0 |K:94|)
        |                       (= (+ |K:90| |K:94|) |K:99|)
        |                       (<= 0 |K:99|)
        |                       (<= 0 |R':96|)
        |                       (<= 0 |D100':98|)
        |                       (<= 0 |C1':97|)
        |                       (<= 0 |i':95|)
        |                       (= (+ (- 0 |i':95|) |R':96|) 0)
        |                       (= (+ (- 0 |i':95|) |C1':97|) 0)
        |                       (< |i':95| |param0:12|)
        |                       (= D100 1)
        |                       (= C1 (+ |C1':97| 1))
        |                       (= R |R':96|)
        |                       (= i (+ |i':95| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (j Int)
        |                    (i Int)
        |                    (R Int)
        |                    (|C1':165| Int)
        |                    (|R':174| Int)
        |                    (|K:175| Int)
        |                    (|j':172| Int)
        |                    (|param0:14| Int)
        |                    (|param1:17| Int)
        |                    (|K:161| Int)
        |                    (|i':162| Int)
        |                    (|mid_D100:168| Int)
        |                    (|mid_D100:157| Int)
        |                    (|K:171| Int)
        |                    (|R':160| Int)
        |                    (|mid_C1:159| Int)
        |                    (|K:169| Int)
        |                    (|mid_j:158| Int)
        |                    (|D100':163| Int)
        |                    (|mid_j:167| Int)
        |                    (|K:155| Int)
        |                    (|K:166| Int)
        |                    (|mid_R:156| Int)
        |                    (|D100':173| Int)
        |                    (|j':164| Int)
        |                    (|j:3| Int)
        |                    (|mid_i:154| Int)
        |                    (|mid_R:170| Int))
        |             (! (let ((a!1 (= |mid_j:158| (* (ite (= |K:155| 0) 1 0) |j:3|)))
        |                      (a!2 (and (<= 1 |K:155|)
        |                                (<= 0 (+ (- 0 1) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (= (+ (- 0 |mid_C1:159|) |mid_i:154|) 0)
        |                                (= |mid_j:158| 0)
        |                                (= |mid_D100:157| 0)
        |                                (<= 0 (+ (- 0 |mid_C1:159|) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (<= 0 |mid_R:156|)
        |                                (<= 0 (+ (- 0 1) |mid_C1:159|))))
        |                      (a!3 (not (<= (+ (- 0 |param1:17|) 1) 0)))
        |                      (a!4 (+ (* (ite (= |K:161| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:161| 0) 1 0) |mid_D100:157|)))
        |                      (a!5 (+ (* (ite (= |K:161| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:161| 0) 1 0) |mid_j:158|)))
        |                      (a!6 (= (+ |i':162| (- 0 |C1':165|))
        |                              (* (ite (= |K:161| 0) 1 0)
        |                                 (+ |mid_i:154| (- 0 |mid_C1:159|)))))
        |                      (a!7 (and (<= 1 |K:161|)
        |                                (= (+ (- 0 |mid_i:154|) |mid_C1:159|) 0)
        |                                (<= 0 (+ (- 0 1) (- 0 |mid_i:154|) |param0:14|))
        |                                (<= 0 |mid_R:156|)
        |                                (<= 0 (+ (- 0 1) |param1:17|))
        |                                (<= 0 |mid_D100:157|)
        |                                (<= 0 |mid_i:154|)
        |                                (= (+ (- 0 |C1':165|) |i':162|) 0)
        |                                (= (+ (- 0 |j':164|) |D100':163|) 0)
        |                                (= (+ (- 0 |j':164|) |param1:17|) 0)
        |                                (<= 0 (+ (- 0 |C1':165|) |param0:14|))
        |                                (<= 0 (+ (- 0 |j':164|) |R':160|))
        |                                (<= 0 (+ (- 0 1) |j':164|))
        |                                (<= 0 (+ (- 0 1) |C1':165|))))
        |                      (a!8 (or (= 0 |K:161|) (<= (+ (- 0 |param1:17|) 1) 0)))
        |                      (a!9 (and (<= 1 |K:169|)
        |                                (<= 0 (+ (- 0 1) |param1:17|))
        |                                (<= 0 |R':160|)
        |                                (= (+ (- 0 |mid_D100:168|) |mid_j:167|) 0)
        |                                (<= 0 (+ (- 0 |mid_D100:168|) |param1:17|))
        |                                (<= 0 (+ (- 0 1) |mid_R:170|))
        |                                (<= 0 (+ (- 0 1) |mid_D100:168|)))))
        |                  (and (= |mid_i:154| |K:155|)
        |                       (= |mid_R:156| 0)
        |                       (= |mid_D100:157| 0)
        |                       a!1
        |                       (= (+ (- 0 |mid_i:154|) |mid_C1:159|) 0)
        |                       (<= |mid_D100:157| 0)
        |                       (= |K:155| |K:155|)
        |                       (or (and (= |K:155| 0)
        |                                (= |j:3| |mid_j:158|)
        |                                (= 0 |mid_i:154|)
        |                                (= 0 |mid_D100:157|)
        |                                (= 0 |mid_C1:159|)
        |                                (= 0 |mid_R:156|))
        |                           a!2)
        |                       (or (= 0 |K:155|) a!3)
        |                       (= |R':160| (+ (* |K:161| |param1:17|) |mid_R:156|))
        |                       (= |i':162| (+ |K:161| |mid_i:154|))
        |                       (= |D100':163| a!4)
        |                       (= |j':164| a!5)
        |                       a!6
        |                       (<= |D100':163|
        |                           (+ (* |K:161| |param1:17|) |mid_D100:157|))
        |                       (= |K:161| |K:161|)
        |                       (or (and (= |K:161| 0)
        |                                (= |mid_j:158| |j':164|)
        |                                (= |mid_i:154| |i':162|)
        |                                (= |mid_D100:157| |D100':163|)
        |                                (= |mid_C1:159| |C1':165|)
        |                                (= |mid_R:156| |R':160|))
        |                           a!7)
        |                       a!8
        |                       (= (+ |K:155| |K:161|) |K:166|)
        |                       (<= 0 |K:166|)
        |                       (<= 0 |D100':163|)
        |                       (<= 0 |R':160|)
        |                       (<= 0 |C1':165|)
        |                       (<= 0 |i':162|)
        |                       (= (+ (- 0 |i':162|) |C1':165|) 0)
        |                       (< |i':162| |param0:14|)
        |                       (= (+ (- 0 |mid_j:167|) |mid_D100:168|) 0)
        |                       (= |mid_j:167| |K:169|)
        |                       (= |mid_R:170| (+ |K:169| |R':160|))
        |                       (= |K:169| |K:169|)
        |                       (or (and (= |K:169| 0)
        |                                (= 0 |mid_j:167|)
        |                                (= 0 |mid_D100:168|)
        |                                (= |R':160| |mid_R:170|))
        |                           a!9)
        |                       (= |K:171| |K:171|)
        |                       (= |K:171| 0)
        |                       (= |mid_j:167| |j':172|)
        |                       (= |mid_D100:168| |D100':173|)
        |                       (= |mid_R:170| |R':174|)
        |                       (= 0 |K:171|)
        |                       (= (+ |K:169| |K:171|) |K:175|)
        |                       (<= 0 |K:175|)
        |                       (<= 0 |D100':173|)
        |                       (<= 0 |R':174|)
        |                       (<= 0 |j':172|)
        |                       (= (+ (- 0 |j':172|) |D100':173|) 0)
        |                       (< |j':172| |param1:17|)
        |                       (= R (+ |R':174| 1))
        |                       (= C1 (+ |C1':165| 1))
        |                       (= D100 (+ |D100':173| 1))
        |                       (= i |i':162|)
        |                       (= j (+ |j':172| 1))
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
        |                    (j Int)
        |                    (i Int)
        |                    (R Int)
        |                    (|R':97| Int)
        |                    (|D100':99| Int)
        |                    (|K:91| Int)
        |                    (|mid_R:93| Int)
        |                    (|mid_D100:92| Int)
        |                    (|param0:12| Int)
        |                    (|K:95| Int)
        |                    (|mid_C1:94| Int)
        |                    (|K:100| Int)
        |                    (|mid_i:90| Int)
        |                    (|C1':98| Int)
        |                    (|i':96| Int))
        |             (! (let ((a!1 (and (<= 1 |K:91|)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |mid_R:93|) |mid_C1:94|) 0)
        |                                (= (+ (- 0 |mid_R:93|) |mid_i:90|) 0)
        |                                (= (+ (- 0 1) |mid_D100:92|) 0)
        |                                (<= 0 (+ (- 0 |mid_R:93|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |mid_R:93|)))))
        |                  (and (= |mid_i:90| |K:91|)
        |                       (= |mid_D100:92| (ite (= |K:91| 0) 0 1))
        |                       (= (+ |mid_i:90| (- 0 |mid_R:93|)) 0)
        |                       (= (+ |mid_C1:94| (- 0 |mid_R:93|)) 0)
        |                       (<= |mid_D100:92| |K:91|)
        |                       (= |K:91| |K:91|)
        |                       (or (and (= |K:91| 0)
        |                                (= 0 |mid_i:90|)
        |                                (= 0 |mid_R:93|)
        |                                (= 0 |mid_C1:94|)
        |                                (= 0 |mid_D100:92|))
        |                           a!1)
        |                       (= |K:95| |K:95|)
        |                       (= |K:95| 0)
        |                       (= |mid_i:90| |i':96|)
        |                       (= |mid_R:93| |R':97|)
        |                       (= |mid_C1:94| |C1':98|)
        |                       (= |mid_D100:92| |D100':99|)
        |                       (= 0 |K:95|)
        |                       (= (+ |K:91| |K:95|) |K:100|)
        |                       (<= 0 |K:100|)
        |                       (<= 0 |R':97|)
        |                       (<= 0 |D100':99|)
        |                       (<= 0 |C1':98|)
        |                       (<= 0 |i':96|)
        |                       (= (+ (- 0 |i':96|) |R':97|) 0)
        |                       (= (+ (- 0 |i':96|) |C1':98|) 0)
        |                       (< |i':96| |param0:12|)
        |                       (= D100 |D100':99|)
        |                       (= C1 (+ |C1':98| 1))
        |                       (= R |R':97|)
        |                       (= i (+ |i':96| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (j Int)
        |                    (i Int)
        |                    (R Int)
        |                    (|C1':165| Int)
        |                    (|param0:14| Int)
        |                    (|param1:17| Int)
        |                    (|K:161| Int)
        |                    (|i':162| Int)
        |                    (|mid_D100:157| Int)
        |                    (|R':160| Int)
        |                    (|mid_C1:159| Int)
        |                    (|mid_j:158| Int)
        |                    (|D100':163| Int)
        |                    (|K:155| Int)
        |                    (|K:166| Int)
        |                    (|mid_R:156| Int)
        |                    (|j':164| Int)
        |                    (|j:3| Int)
        |                    (|mid_i:154| Int))
        |             (! (let ((a!1 (= |mid_j:158| (* (ite (= |K:155| 0) 1 0) |j:3|)))
        |                      (a!2 (and (<= 1 |K:155|)
        |                                (<= 0 (+ (- 0 1) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (= (+ (- 0 |mid_C1:159|) |mid_i:154|) 0)
        |                                (= |mid_j:158| 0)
        |                                (= |mid_D100:157| 0)
        |                                (<= 0 (+ (- 0 |mid_C1:159|) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (<= 0 |mid_R:156|)
        |                                (<= 0 (+ (- 0 1) |mid_C1:159|))))
        |                      (a!3 (not (<= (+ (- 0 |param1:17|) 1) 0)))
        |                      (a!4 (+ (* (ite (= |K:161| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:161| 0) 1 0) |mid_D100:157|)))
        |                      (a!5 (+ (* (ite (= |K:161| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:161| 0) 1 0) |mid_j:158|)))
        |                      (a!6 (= (+ |i':162| (- 0 |C1':165|))
        |                              (* (ite (= |K:161| 0) 1 0)
        |                                 (+ |mid_i:154| (- 0 |mid_C1:159|)))))
        |                      (a!7 (and (<= 1 |K:161|)
        |                                (= (+ (- 0 |mid_i:154|) |mid_C1:159|) 0)
        |                                (<= 0 (+ (- 0 1) (- 0 |mid_i:154|) |param0:14|))
        |                                (<= 0 |mid_R:156|)
        |                                (<= 0 (+ (- 0 1) |param1:17|))
        |                                (<= 0 |mid_D100:157|)
        |                                (<= 0 |mid_i:154|)
        |                                (= (+ (- 0 |C1':165|) |i':162|) 0)
        |                                (= (+ (- 0 |j':164|) |D100':163|) 0)
        |                                (= (+ (- 0 |j':164|) |param1:17|) 0)
        |                                (<= 0 (+ (- 0 |C1':165|) |param0:14|))
        |                                (<= 0 (+ (- 0 |j':164|) |R':160|))
        |                                (<= 0 (+ (- 0 1) |j':164|))
        |                                (<= 0 (+ (- 0 1) |C1':165|))))
        |                      (a!8 (or (= 0 |K:161|) (<= (+ (- 0 |param1:17|) 1) 0))))
        |                  (and (= |mid_i:154| |K:155|)
        |                       (= |mid_R:156| 0)
        |                       (= |mid_D100:157| 0)
        |                       a!1
        |                       (= (+ (- 0 |mid_i:154|) |mid_C1:159|) 0)
        |                       (<= |mid_D100:157| 0)
        |                       (= |K:155| |K:155|)
        |                       (or (and (= |K:155| 0)
        |                                (= |j:3| |mid_j:158|)
        |                                (= 0 |mid_i:154|)
        |                                (= 0 |mid_D100:157|)
        |                                (= 0 |mid_C1:159|)
        |                                (= 0 |mid_R:156|))
        |                           a!2)
        |                       (or (= 0 |K:155|) a!3)
        |                       (= |R':160| (+ (* |K:161| |param1:17|) |mid_R:156|))
        |                       (= |i':162| (+ |K:161| |mid_i:154|))
        |                       (= |D100':163| a!4)
        |                       (= |j':164| a!5)
        |                       a!6
        |                       (<= |D100':163|
        |                           (+ (* |K:161| |param1:17|) |mid_D100:157|))
        |                       (= |K:161| |K:161|)
        |                       (or (and (= |K:161| 0)
        |                                (= |mid_j:158| |j':164|)
        |                                (= |mid_i:154| |i':162|)
        |                                (= |mid_D100:157| |D100':163|)
        |                                (= |mid_C1:159| |C1':165|)
        |                                (= |mid_R:156| |R':160|))
        |                           a!7)
        |                       a!8
        |                       (= (+ |K:155| |K:161|) |K:166|)
        |                       (<= 0 |K:166|)
        |                       (<= 0 |D100':163|)
        |                       (<= 0 |R':160|)
        |                       (<= 0 |C1':165|)
        |                       (<= 0 |i':162|)
        |                       (= (+ (- 0 |i':162|) |C1':165|) 0)
        |                       (< |i':162| |param0:14|)
        |                       (= R |R':160|)
        |                       (= C1 (+ |C1':165| 1))
        |                       (= D100 |D100':163|)
        |                       (= i |i':162|)
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
      """(let ((a!1 (exists ((j Int)
        |                    (i Int)
        |                    (D100 Int)
        |                    (R Int)
        |                    (|R':97| Int)
        |                    (|D100':99| Int)
        |                    (|K:91| Int)
        |                    (|mid_R:93| Int)
        |                    (|mid_D100:92| Int)
        |                    (|param0:12| Int)
        |                    (|K:95| Int)
        |                    (|mid_C1:94| Int)
        |                    (|K:100| Int)
        |                    (|mid_i:90| Int)
        |                    (|C1':98| Int)
        |                    (|i':96| Int))
        |             (! (let ((a!1 (and (<= 1 |K:91|)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |mid_R:93|) |mid_C1:94|) 0)
        |                                (= (+ (- 0 |mid_R:93|) |mid_i:90|) 0)
        |                                (= (+ (- 0 1) |mid_D100:92|) 0)
        |                                (<= 0 (+ (- 0 |mid_R:93|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |mid_R:93|)))))
        |                  (and (= |mid_i:90| |K:91|)
        |                       (= |mid_D100:92| (ite (= |K:91| 0) 0 1))
        |                       (= (+ |mid_i:90| (- 0 |mid_R:93|)) 0)
        |                       (= (+ |mid_C1:94| (- 0 |mid_R:93|)) 0)
        |                       (<= |mid_D100:92| |K:91|)
        |                       (= |K:91| |K:91|)
        |                       (or (and (= |K:91| 0)
        |                                (= 0 |mid_i:90|)
        |                                (= 0 |mid_R:93|)
        |                                (= 0 |mid_C1:94|)
        |                                (= 0 |mid_D100:92|))
        |                           a!1)
        |                       (= |K:95| |K:95|)
        |                       (= |K:95| 0)
        |                       (= |mid_i:90| |i':96|)
        |                       (= |mid_R:93| |R':97|)
        |                       (= |mid_C1:94| |C1':98|)
        |                       (= |mid_D100:92| |D100':99|)
        |                       (= 0 |K:95|)
        |                       (= (+ |K:91| |K:95|) |K:100|)
        |                       (<= 0 |K:100|)
        |                       (<= 0 |R':97|)
        |                       (<= 0 |D100':99|)
        |                       (<= 0 |C1':98|)
        |                       (<= 0 |i':96|)
        |                       (= (+ (- 0 |i':96|) |R':97|) 0)
        |                       (= (+ (- 0 |i':96|) |C1':98|) 0)
        |                       (< |i':96| |param0:12|)
        |                       (= D100 |D100':99|)
        |                       (= C1 (+ |C1':98| 1))
        |                       (= R |R':97|)
        |                       (= i (+ |i':96| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((j Int)
        |                    (i Int)
        |                    (D100 Int)
        |                    (R Int)
        |                    (|C1':165| Int)
        |                    (|param0:14| Int)
        |                    (|param1:17| Int)
        |                    (|K:161| Int)
        |                    (|i':162| Int)
        |                    (|mid_D100:157| Int)
        |                    (|R':160| Int)
        |                    (|mid_C1:159| Int)
        |                    (|mid_j:158| Int)
        |                    (|D100':163| Int)
        |                    (|K:155| Int)
        |                    (|K:166| Int)
        |                    (|mid_R:156| Int)
        |                    (|j':164| Int)
        |                    (|j:3| Int)
        |                    (|mid_i:154| Int))
        |             (! (let ((a!1 (= |mid_j:158| (* (ite (= |K:155| 0) 1 0) |j:3|)))
        |                      (a!2 (and (<= 1 |K:155|)
        |                                (<= 0 (+ (- 0 1) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (= (+ (- 0 |mid_C1:159|) |mid_i:154|) 0)
        |                                (= |mid_j:158| 0)
        |                                (= |mid_D100:157| 0)
        |                                (<= 0 (+ (- 0 |mid_C1:159|) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (<= 0 |mid_R:156|)
        |                                (<= 0 (+ (- 0 1) |mid_C1:159|))))
        |                      (a!3 (not (<= (+ (- 0 |param1:17|) 1) 0)))
        |                      (a!4 (+ (* (ite (= |K:161| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:161| 0) 1 0) |mid_D100:157|)))
        |                      (a!5 (+ (* (ite (= |K:161| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:161| 0) 1 0) |mid_j:158|)))
        |                      (a!6 (= (+ |i':162| (- 0 |C1':165|))
        |                              (* (ite (= |K:161| 0) 1 0)
        |                                 (+ |mid_i:154| (- 0 |mid_C1:159|)))))
        |                      (a!7 (and (<= 1 |K:161|)
        |                                (= (+ (- 0 |mid_i:154|) |mid_C1:159|) 0)
        |                                (<= 0 (+ (- 0 1) (- 0 |mid_i:154|) |param0:14|))
        |                                (<= 0 |mid_R:156|)
        |                                (<= 0 (+ (- 0 1) |param1:17|))
        |                                (<= 0 |mid_D100:157|)
        |                                (<= 0 |mid_i:154|)
        |                                (= (+ (- 0 |C1':165|) |i':162|) 0)
        |                                (= (+ (- 0 |j':164|) |D100':163|) 0)
        |                                (= (+ (- 0 |j':164|) |param1:17|) 0)
        |                                (<= 0 (+ (- 0 |C1':165|) |param0:14|))
        |                                (<= 0 (+ (- 0 |j':164|) |R':160|))
        |                                (<= 0 (+ (- 0 1) |j':164|))
        |                                (<= 0 (+ (- 0 1) |C1':165|))))
        |                      (a!8 (or (= 0 |K:161|) (<= (+ (- 0 |param1:17|) 1) 0))))
        |                  (and (= |mid_i:154| |K:155|)
        |                       (= |mid_R:156| 0)
        |                       (= |mid_D100:157| 0)
        |                       a!1
        |                       (= (+ (- 0 |mid_i:154|) |mid_C1:159|) 0)
        |                       (<= |mid_D100:157| 0)
        |                       (= |K:155| |K:155|)
        |                       (or (and (= |K:155| 0)
        |                                (= |j:3| |mid_j:158|)
        |                                (= 0 |mid_i:154|)
        |                                (= 0 |mid_D100:157|)
        |                                (= 0 |mid_C1:159|)
        |                                (= 0 |mid_R:156|))
        |                           a!2)
        |                       (or (= 0 |K:155|) a!3)
        |                       (= |R':160| (+ (* |K:161| |param1:17|) |mid_R:156|))
        |                       (= |i':162| (+ |K:161| |mid_i:154|))
        |                       (= |D100':163| a!4)
        |                       (= |j':164| a!5)
        |                       a!6
        |                       (<= |D100':163|
        |                           (+ (* |K:161| |param1:17|) |mid_D100:157|))
        |                       (= |K:161| |K:161|)
        |                       (or (and (= |K:161| 0)
        |                                (= |mid_j:158| |j':164|)
        |                                (= |mid_i:154| |i':162|)
        |                                (= |mid_D100:157| |D100':163|)
        |                                (= |mid_C1:159| |C1':165|)
        |                                (= |mid_R:156| |R':160|))
        |                           a!7)
        |                       a!8
        |                       (= (+ |K:155| |K:161|) |K:166|)
        |                       (<= 0 |K:166|)
        |                       (<= 0 |D100':163|)
        |                       (<= 0 |R':160|)
        |                       (<= 0 |C1':165|)
        |                       (<= 0 |i':162|)
        |                       (= (+ (- 0 |i':162|) |C1':165|) 0)
        |                       (< |i':162| |param0:14|)
        |                       (= R |R':160|)
        |                       (= C1 (+ |C1':165| 1))
        |                       (= D100 |D100':163|)
        |                       (= i |i':162|)
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

  val boundCheckingTests: List[(TestCaseJavaProgram, Z3Solver, AST)] = {
    val TRUE = "true"
    val FALSE = "false"

    val solver1 = new Z3Solver
    val boundExpression1 = solver1.mkLe(
      solver1.mkIntVar("R"),
      solver1.mkIntVar("n")
    )

    val solver2 = new Z3Solver
    val boundExpression2 = {
      val R = solver2.mkIntVar("R")
      solver2.mkITE(
        solver2.mkGt(R, solver2.mkIntVal(1)),
        solver2.mkLe(
          R,
          solver2.mkIntVal(1)
        ),
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

    List[(TestCaseJavaProgram, Z3Solver, AST)](
      (TestCaseJavaProgram("Test01", test01, TRUE), solver1, boundExpression1),
      (TestCaseJavaProgram("Test01", test01, FALSE), solver2, boundExpression2),
      (TestCaseJavaProgram("Test02", test02, TRUE), solver3, boundExpression3),
      (TestCaseJavaProgram("Test02", test02, FALSE), solver4, boundExpression4),
      (TestCaseJavaProgram("Test03", test03, TRUE), solver5, boundExpression5),
      (TestCaseJavaProgram("Test03", test03, FALSE), solver6, boundExpression6),
      (TestCaseJavaProgram("Test04", test04, TRUE), solver7, boundExpression7),
      (TestCaseJavaProgram("Test04", test04, FALSE), solver8, boundExpression8),
    )
  }
}