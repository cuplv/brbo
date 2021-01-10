package brbo.boundinference

import brbo.TestCaseJavaProgram
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashSet

class BoundCheckingUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[BoundCheckingUnitTest])

  "Extracting bound expression" should "succeed" in {

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
      |    D100 = D100 + 1;
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
      |
      |    for (int j = 0; j < l; j++) {
      |      R++;
      |      D100++;
      |    }
      |  }
      |}""".stripMargin

  val deltaVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected =
      """(let ((a!1 (exists ((C1 Int) (j Int) (i Int) (R Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:76|) (<= |K:76| 0)))
        |                               (= |R':77| |K:76|)))
        |                      (a!2 (or (not (and (<= 0 |K:76|) (<= |K:76| 0)))
        |                               (= |D100':78| 1)))
        |                      (a!3 (or (not (and (<= 0 |K:76|) (<= |K:76| 0)))
        |                               (= |C1':79| |K:76|)))
        |                      (a!4 (and (<= 1 |K:76|)
        |                                (= (+ (- 0 1) 1) 0)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |R':77|) |C1':79|) 0)
        |                                (= (+ (- 0 |R':77|) |i':80|) 0)
        |                                (= (+ (- 0 1) |D100':78|) 0)
        |                                (<= 0 (+ (- 0 |R':77|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |R':77|)))))
        |                  (and a!1
        |                       (or (not (<= 1 |K:76|)) (= |R':77| |K:76|))
        |                       a!2
        |                       (or (not (<= 1 |K:76|)) (= |D100':78| 1))
        |                       a!3
        |                       (or (not (<= 1 |K:76|)) (= |C1':79| |K:76|))
        |                       (or (not (<= 0 |K:76|)) (= |i':80| |K:76|))
        |                       (or (and (= |K:76| 0)
        |                                (= 0 |i':80|)
        |                                (= 0 |R':77|)
        |                                (= 0 |C1':79|)
        |                                (= 1 |D100':78|))
        |                           a!4)
        |                       (<= 0 |K:76|)
        |                       (<= 0 |R':77|)
        |                       (<= 0 |C1':79|)
        |                       (<= 0 |i':80|)
        |                       (< 0 |D100':78|)
        |                       (= (+ (- 0 |i':80|) |R':77|) 0)
        |                       (= (+ (- 0 |i':80|) |C1':79|) 0)
        |                       (= (+ (- 0 1) |D100':78|) 0)
        |                       (< |i':80| |param0:12|)
        |                       (= D100 1)
        |                       (= C1 (+ |C1':79| 1))
        |                       (= R |R':77|)
        |                       (= i (+ |i':80| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (and (exists ((C1 Int) (j Int) (i Int) (R Int))
        |         (! (and (= 0 0) (= D100 1) (= C1 0) (= R 0) (= i 0) (= n |param0:12|))
        |            :weight 0))
        |       a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((C1 Int) (j Int) (i Int) (R Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:125|) (<= |K:125| 0)))
        |                               (= |C1':127| |K:125|)))
        |                      (a!2 (or (not (and (<= 0 |K:125|) (<= |K:125| 0)))
        |                               (= (+ |D100':128| (- 0 |j':129|)) (- 0 |j:3|))))
        |                      (a!3 (or (not (<= 1 |K:125|))
        |                               (= (+ |D100':128| (- 0 |j':129|)) 0)))
        |                      (a!4 (or (not (<= 0 |K:125|))
        |                               (<= (+ (- 0 |R':130|) |D100':128|) 0)))
        |                      (a!5 (or (not (<= 0 |K:125|))
        |                               (<= (- 0 |R':130|) (- 0 (* |param1:19| |K:125|)))))
        |                      (a!6 (or (not (<= 0 |K:125|)) (<= (- 0 |R':130|) 0)))
        |                      (a!7 (and (<= 1 |K:125|)
        |                                (<= 0 (+ (- 0 1) |param0:16|))
        |                                (= (+ (- 0 |j':129|) |D100':128|) 0)
        |                                (= (+ (- 0 |i':126|) |C1':127|) 0)
        |                                (<= 0 (+ (- 0 |j':129|) |R':130|))
        |                                (<= 0 (+ (- 0 |i':126|) |param0:16|))
        |                                (<= 0 (+ (- 0 1) |i':126|))
        |                                (<= 0 (+ |j':129| (- 0 |param1:19|)))
        |                                (<= 0 |j':129|)))
        |                      (a!9 (or (not (and (<= 0 |K:147|) (<= |K:147| 0)))
        |                               (= |D100':149| |K:147|)))
        |                      (a!10 (and (<= 1 |K:147|)
        |                                 (<= 0 (+ (- 0 1) |param1:19|))
        |                                 (<= 0 |R':130|)
        |                                 (= (+ (- 0 |D100':149|) |j':148|) 0)
        |                                 (<= 0 (+ (- 0 |D100':149|) |param1:19|))
        |                                 (<= 0 (+ (- 0 1) |R':150|))
        |                                 (<= 0 (+ (- 0 1) |D100':149|)))))
        |                (let ((a!8 (or (and (= |K:125| 0)
        |                                    (= |j:3| |j':129|)
        |                                    (= 0 |i':126|)
        |                                    (= 0 |D100':128|)
        |                                    (= 0 |C1':127|)
        |                                    (= 0 |R':130|))
        |                               a!7)))
        |                  (and (or (not (<= 0 |K:125|)) (= |i':126| |K:125|))
        |                       a!1
        |                       (or (not (<= 1 |K:125|)) (= |C1':127| |K:125|))
        |                       a!2
        |                       a!3
        |                       a!4
        |                       a!5
        |                       a!6
        |                       a!8
        |                       (<= 0 |K:125|)
        |                       (<= 0 |D100':128|)
        |                       (<= 0 |R':130|)
        |                       (<= 0 |C1':127|)
        |                       (<= 0 |i':126|)
        |                       (= (+ (- 0 |i':126|) |C1':127|) 0)
        |                       (< |i':126| |param0:16|)
        |                       (or (not (<= 0 |K:147|)) (= |j':148| |K:147|))
        |                       a!9
        |                       (or (not (<= 1 |K:147|)) (= |D100':149| |K:147|))
        |                       (or (not (<= 0 |K:147|))
        |                           (= |R':150| (+ |R':130| |K:147|)))
        |                       (or (and (= |K:147| 0)
        |                                (= 0 |j':148|)
        |                                (= 0 |D100':149|)
        |                                (= |R':130| |R':150|))
        |                           a!10)
        |                       (<= 0 |K:147|)
        |                       (<= 0 |D100':149|)
        |                       (<= 0 |R':150|)
        |                       (<= 0 |j':148|)
        |                       (= (+ (- 0 |j':148|) |D100':149|) 0)
        |                       (< |j':148| |param1:19|)
        |                       (= R (+ |R':150| 1))
        |                       (= C1 (+ |C1':127| 1))
        |                       (= D100 (+ |D100':149| 1))
        |                       (= i |i':126|)
        |                       (= j (+ |j':148| 1))
        |                       (= n |param0:16|)
        |                       (= m |param1:19|)
        |                       (= l |param2:22|))))
        |                :weight 0)))
        |      (a!2 (exists ((C1 Int) (j Int) (i Int) (R Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:125|) (<= |K:125| 0)))
        |                               (= |C1':127| |K:125|)))
        |                      (a!2 (or (not (and (<= 0 |K:125|) (<= |K:125| 0)))
        |                               (= (+ |D100':128| (- 0 |j':129|)) (- 0 |j:3|))))
        |                      (a!3 (or (not (<= 1 |K:125|))
        |                               (= (+ |D100':128| (- 0 |j':129|)) 0)))
        |                      (a!4 (or (not (<= 0 |K:125|))
        |                               (<= (+ (- 0 |R':130|) |D100':128|) 0)))
        |                      (a!5 (or (not (<= 0 |K:125|))
        |                               (<= (- 0 |R':130|) (- 0 (* |param1:19| |K:125|)))))
        |                      (a!6 (or (not (<= 0 |K:125|)) (<= (- 0 |R':130|) 0)))
        |                      (a!7 (and (<= 1 |K:125|)
        |                                (<= 0 (+ (- 0 1) |param0:16|))
        |                                (= (+ (- 0 |j':129|) |D100':128|) 0)
        |                                (= (+ (- 0 |i':126|) |C1':127|) 0)
        |                                (<= 0 (+ (- 0 |j':129|) |R':130|))
        |                                (<= 0 (+ (- 0 |i':126|) |param0:16|))
        |                                (<= 0 (+ (- 0 1) |i':126|))
        |                                (<= 0 (+ |j':129| (- 0 |param1:19|)))
        |                                (<= 0 |j':129|)))
        |                      (a!9 (and (<= 1 |K:140|)
        |                                (<= 0 (+ (- 0 1) |param2:22|))
        |                                (<= 0 |R':130|)
        |                                (<= 0 |D100':128|)
        |                                (<= 0 (+ (- 0 |j___0':142|) |param2:22|))
        |                                (<= 0 (+ (- 0 1) |R':141|))
        |                                (<= 0 (+ (- 0 1) |D100':143|))
        |                                (<= 0 (+ (- 0 1) |j___0':142|)))))
        |                (let ((a!8 (or (and (= |K:125| 0)
        |                                    (= |j:3| |j':129|)
        |                                    (= 0 |i':126|)
        |                                    (= 0 |D100':128|)
        |                                    (= 0 |C1':127|)
        |                                    (= 0 |R':130|))
        |                               a!7)))
        |                  (and (or (not (<= 0 |K:125|)) (= |i':126| |K:125|))
        |                       a!1
        |                       (or (not (<= 1 |K:125|)) (= |C1':127| |K:125|))
        |                       a!2
        |                       a!3
        |                       a!4
        |                       a!5
        |                       a!6
        |                       a!8
        |                       (<= 0 |K:125|)
        |                       (<= 0 |D100':128|)
        |                       (<= 0 |R':130|)
        |                       (<= 0 |C1':127|)
        |                       (<= 0 |i':126|)
        |                       (= (+ (- 0 |i':126|) |C1':127|) 0)
        |                       (<= |param0:16| |i':126|)
        |                       (or (not (<= 0 |K:140|))
        |                           (= |R':141| (+ |R':130| |K:140|)))
        |                       (or (not (<= 0 |K:140|)) (= |j___0':142| |K:140|))
        |                       (or (not (<= 0 |K:140|))
        |                           (= |D100':143| (+ |D100':128| |K:140|)))
        |                       (or (and (= |K:140| 0)
        |                                (= 0 |j___0':142|)
        |                                (= |D100':128| |D100':143|)
        |                                (= |R':130| |R':141|))
        |                           a!9)
        |                       (<= 0 |K:140|)
        |                       (<= 0 |j___0':142|)
        |                       (<= 0 |D100':143|)
        |                       (<= 0 |R':141|)
        |                       (< |j___0':142| |param2:22|)
        |                       (= R (+ |R':141| 1))
        |                       (= C1 |C1':127|)
        |                       (= D100 (+ |D100':143| 1))
        |                       (= i |i':126|)
        |                       (= j |j':129|)
        |                       (= j___0 |j___0':142|)
        |                       (= n |param0:16|)
        |                       (= m |param1:19|)
        |                       (= l |param2:22|))))
        |                :weight 0))))
        |  (and a!1 a!2))""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01Expected),
      TestCaseJavaProgram("Test02", test02, test02Expected),
    )
  }

  val deltaVariableResetTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected =
      """(let ((a!1 (exists ((C1 Int) (j Int) (i Int) (R Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:76|) (<= |K:76| 0)))
        |                               (= |R':77| |K:76|)))
        |                      (a!2 (or (not (and (<= 0 |K:76|) (<= |K:76| 0)))
        |                               (= |D100':78| 1)))
        |                      (a!3 (or (not (and (<= 0 |K:76|) (<= |K:76| 0)))
        |                               (= |C1':79| |K:76|)))
        |                      (a!4 (and (<= 1 |K:76|)
        |                                (= (+ (- 0 1) 1) 0)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |R':77|) |C1':79|) 0)
        |                                (= (+ (- 0 |R':77|) |i':80|) 0)
        |                                (= (+ (- 0 1) |D100':78|) 0)
        |                                (<= 0 (+ (- 0 |R':77|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |R':77|)))))
        |                  (and a!1
        |                       (or (not (<= 1 |K:76|)) (= |R':77| |K:76|))
        |                       a!2
        |                       (or (not (<= 1 |K:76|)) (= |D100':78| 1))
        |                       a!3
        |                       (or (not (<= 1 |K:76|)) (= |C1':79| |K:76|))
        |                       (or (not (<= 0 |K:76|)) (= |i':80| |K:76|))
        |                       (or (and (= |K:76| 0)
        |                                (= 0 |i':80|)
        |                                (= 0 |R':77|)
        |                                (= 0 |C1':79|)
        |                                (= 1 |D100':78|))
        |                           a!4)
        |                       (<= 0 |K:76|)
        |                       (<= 0 |R':77|)
        |                       (<= 0 |C1':79|)
        |                       (<= 0 |i':80|)
        |                       (< 0 |D100':78|)
        |                       (= (+ (- 0 |i':80|) |R':77|) 0)
        |                       (= (+ (- 0 |i':80|) |C1':79|) 0)
        |                       (= (+ (- 0 1) |D100':78|) 0)
        |                       (< |i':80| |param0:12|)
        |                       (= D100 |D100':78|)
        |                       (= C1 (+ |C1':79| 1))
        |                       (= R |R':77|)
        |                       (= i (+ |i':80| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (and a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((C1 Int) (j Int) (i Int) (R Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:125|) (<= |K:125| 0)))
        |                               (= |C1':127| |K:125|)))
        |                      (a!2 (or (not (and (<= 0 |K:125|) (<= |K:125| 0)))
        |                               (= (+ |D100':128| (- 0 |j':129|)) (- 0 |j:3|))))
        |                      (a!3 (or (not (<= 1 |K:125|))
        |                               (= (+ |D100':128| (- 0 |j':129|)) 0)))
        |                      (a!4 (or (not (<= 0 |K:125|))
        |                               (<= (+ (- 0 |R':130|) |D100':128|) 0)))
        |                      (a!5 (or (not (<= 0 |K:125|))
        |                               (<= (- 0 |R':130|) (- 0 (* |param1:19| |K:125|)))))
        |                      (a!6 (and (<= 1 |K:125|)
        |                                (<= 0 (+ (- 0 1) |param0:16|))
        |                                (= (+ (- 0 |j':129|) |D100':128|) 0)
        |                                (= (+ (- 0 |i':126|) |C1':127|) 0)
        |                                (<= 0 (+ (- 0 |j':129|) |R':130|))
        |                                (<= 0 (+ (- 0 |i':126|) |param0:16|))
        |                                (<= 0 (+ (- 0 1) |i':126|))
        |                                (<= 0 (+ |j':129| (- 0 |param1:19|)))
        |                                (<= 0 |j':129|))))
        |                  (and (or (not (<= 0 |K:125|)) (= |i':126| |K:125|))
        |                       a!1
        |                       (or (not (<= 1 |K:125|)) (= |C1':127| |K:125|))
        |                       a!2
        |                       a!3
        |                       a!4
        |                       a!5
        |                       (or (not (<= 0 |K:125|)) (<= (- 0 |R':130|) 0))
        |                       (or (and (= |K:125| 0)
        |                                (= |j:3| |j':129|)
        |                                (= 0 |i':126|)
        |                                (= 0 |D100':128|)
        |                                (= 0 |C1':127|)
        |                                (= 0 |R':130|))
        |                           a!6)
        |                       (<= 0 |K:125|)
        |                       (<= 0 |D100':128|)
        |                       (<= 0 |R':130|)
        |                       (<= 0 |C1':127|)
        |                       (<= 0 |i':126|)
        |                       (= (+ (- 0 |i':126|) |C1':127|) 0)
        |                       (< |i':126| |param0:16|)
        |                       (= R |R':130|)
        |                       (= C1 (+ |C1':127| 1))
        |                       (= D100 |D100':128|)
        |                       (= i |i':126|)
        |                       (= j 0)
        |                       (= n |param0:16|)
        |                       (= m |param1:19|)
        |                       (= l |param2:22|)))
        |                :weight 0))))
        |  (and a!1))""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01Expected),
      TestCaseJavaProgram("Test02", test02, test02Expected),
    )
  }

  val counterVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected =
      """(let ((a!1 (exists ((C1 Int) (j Int) (i Int) (R Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:76|) (<= |K:76| 0)))
        |                               (= |R':77| |K:76|)))
        |                      (a!2 (or (not (and (<= 0 |K:76|) (<= |K:76| 0)))
        |                               (= |D100':78| 1)))
        |                      (a!3 (or (not (and (<= 0 |K:76|) (<= |K:76| 0)))
        |                               (= |C1':79| |K:76|)))
        |                      (a!4 (and (<= 1 |K:76|)
        |                                (= (+ (- 0 1) 1) 0)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |R':77|) |C1':79|) 0)
        |                                (= (+ (- 0 |R':77|) |i':80|) 0)
        |                                (= (+ (- 0 1) |D100':78|) 0)
        |                                (<= 0 (+ (- 0 |R':77|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |R':77|)))))
        |                  (and a!1
        |                       (or (not (<= 1 |K:76|)) (= |R':77| |K:76|))
        |                       a!2
        |                       (or (not (<= 1 |K:76|)) (= |D100':78| 1))
        |                       a!3
        |                       (or (not (<= 1 |K:76|)) (= |C1':79| |K:76|))
        |                       (or (not (<= 0 |K:76|)) (= |i':80| |K:76|))
        |                       (or (and (= |K:76| 0)
        |                                (= 0 |i':80|)
        |                                (= 0 |R':77|)
        |                                (= 0 |C1':79|)
        |                                (= 1 |D100':78|))
        |                           a!4)
        |                       (<= 0 |K:76|)
        |                       (<= 0 |R':77|)
        |                       (<= 0 |C1':79|)
        |                       (<= 0 |i':80|)
        |                       (< 0 |D100':78|)
        |                       (= (+ (- 0 |i':80|) |R':77|) 0)
        |                       (= (+ (- 0 |i':80|) |C1':79|) 0)
        |                       (= (+ (- 0 1) |D100':78|) 0)
        |                       (< |i':80| |param0:12|)
        |                       (= D100 |D100':78|)
        |                       (= C1 (+ |C1':79| 1))
        |                       (= R |R':77|)
        |                       (= i (+ |i':80| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (and a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((C1 Int) (j Int) (i Int) (R Int))
        |             (! (let ((a!1 (or (not (and (<= 0 |K:125|) (<= |K:125| 0)))
        |                               (= |C1':127| |K:125|)))
        |                      (a!2 (or (not (and (<= 0 |K:125|) (<= |K:125| 0)))
        |                               (= (+ |D100':128| (- 0 |j':129|)) (- 0 |j:3|))))
        |                      (a!3 (or (not (<= 1 |K:125|))
        |                               (= (+ |D100':128| (- 0 |j':129|)) 0)))
        |                      (a!4 (or (not (<= 0 |K:125|))
        |                               (<= (+ (- 0 |R':130|) |D100':128|) 0)))
        |                      (a!5 (or (not (<= 0 |K:125|))
        |                               (<= (- 0 |R':130|) (- 0 (* |param1:19| |K:125|)))))
        |                      (a!6 (and (<= 1 |K:125|)
        |                                (<= 0 (+ (- 0 1) |param0:16|))
        |                                (= (+ (- 0 |j':129|) |D100':128|) 0)
        |                                (= (+ (- 0 |i':126|) |C1':127|) 0)
        |                                (<= 0 (+ (- 0 |j':129|) |R':130|))
        |                                (<= 0 (+ (- 0 |i':126|) |param0:16|))
        |                                (<= 0 (+ (- 0 1) |i':126|))
        |                                (<= 0 (+ |j':129| (- 0 |param1:19|)))
        |                                (<= 0 |j':129|))))
        |                  (and (or (not (<= 0 |K:125|)) (= |i':126| |K:125|))
        |                       a!1
        |                       (or (not (<= 1 |K:125|)) (= |C1':127| |K:125|))
        |                       a!2
        |                       a!3
        |                       a!4
        |                       a!5
        |                       (or (not (<= 0 |K:125|)) (<= (- 0 |R':130|) 0))
        |                       (or (and (= |K:125| 0)
        |                                (= |j:3| |j':129|)
        |                                (= 0 |i':126|)
        |                                (= 0 |D100':128|)
        |                                (= 0 |C1':127|)
        |                                (= 0 |R':130|))
        |                           a!6)
        |                       (<= 0 |K:125|)
        |                       (<= 0 |D100':128|)
        |                       (<= 0 |R':130|)
        |                       (<= 0 |C1':127|)
        |                       (<= 0 |i':126|)
        |                       (= (+ (- 0 |i':126|) |C1':127|) 0)
        |                       (< |i':126| |param0:16|)
        |                       (= R |R':130|)
        |                       (= C1 (+ |C1':127| 1))
        |                       (= D100 |D100':128|)
        |                       (= i |i':126|)
        |                       (= j 0)
        |                       (= n |param0:16|)
        |                       (= m |param1:19|)
        |                       (= l |param2:22|)))
        |                :weight 0))))
        |  (and a!1))""".stripMargin

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
}