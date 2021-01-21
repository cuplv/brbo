package brbo.common

import brbo.common.BeforeOrAfterOrThis.{AFTER, BEFORE}
import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta}
import brbo.common.TypeUtils.BrboType.{BrboType, INT}
import brbo.verification.{BasicProcessor, BoundCheckingUnitTest}
import brbo.{StringCompare, TestCaseJavaProgram}
import com.sun.source.tree.ExpressionStatementTree
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.node.Node
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.{HashMap, HashSet}

class InvariantInferenceUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[BoundCheckingUnitTest])

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
          HashMap[String, BrboType](
            "R" -> INT,
            "C1" -> INT,
            "i" -> INT,
            "j" -> INT
          ),
          HashMap[String, BrboType](
            "R" -> INT,
            "n" -> INT,
            "m" -> INT,
            "l" -> INT,
            "C1" -> INT,
            "D100" -> INT
          )
        )
        logger.debug(result)
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, testCase.className))
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
          HashMap[String, BrboType](
            "R" -> INT,
            "C1" -> INT,
            "i" -> INT,
            "j" -> INT
          ),
          HashMap[String, BrboType](
            "R" -> INT,
            "n" -> INT,
            "m" -> INT,
            "l" -> INT,
            "C1" -> INT,
            "D100" -> INT
          )
        )
        logger.debug(result)
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, testCase.className))
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
            "R" -> INT,
            "D100" -> INT,
            "i" -> INT,
            "j" -> INT
          ),
          HashMap[String, BrboType](
            "R" -> INT,
            "n" -> INT,
            "m" -> INT,
            "l" -> INT,
            "C1" -> INT,
            "D100" -> INT
          )
        )
        logger.debug(result)
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, testCase.className))
    })
  }
}

object InvariantInferenceUnitTest {
  val deltaVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (j Int)
        |                    (i Int)
        |                    (R Int)
        |                    (|K:146| Int)
        |                    (|mid_R:148| Int)
        |                    (|param0:12| Int)
        |                    (|mid_C1:149| Int)
        |                    (|i':151| Int)
        |                    (|mid_i:145| Int)
        |                    (|R':152| Int)
        |                    (|C1':153| Int)
        |                    (|mid_D100:147| Int)
        |                    (|K:150| Int)
        |                    (|K:155| Int)
        |                    (|D100':154| Int))
        |             (! (let ((a!1 (and (<= 1 |K:146|)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |mid_R:148|) |mid_C1:149|) 0)
        |                                (= (+ (- 0 |mid_R:148|) |mid_i:145|) 0)
        |                                (= (+ (- 0 1) |mid_D100:147|) 0)
        |                                (<= 0 (+ (- 0 |mid_R:148|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |mid_R:148|)))))
        |                  (and (= |mid_i:145| |K:146|)
        |                       (= |mid_D100:147| (ite (= |K:146| 0) 0 1))
        |                       (= (+ |mid_i:145| (- 0 |mid_R:148|)) 0)
        |                       (= (+ |mid_C1:149| (- 0 |mid_R:148|)) 0)
        |                       (<= |mid_D100:147| |K:146|)
        |                       (= |K:146| |K:146|)
        |                       (or (and (= |K:146| 0)
        |                                (= 0 |mid_i:145|)
        |                                (= 0 |mid_R:148|)
        |                                (= 0 |mid_C1:149|)
        |                                (= 0 |mid_D100:147|))
        |                           a!1)
        |                       (= |K:150| |K:150|)
        |                       (= |K:150| 0)
        |                       (= |mid_i:145| |i':151|)
        |                       (= |mid_R:148| |R':152|)
        |                       (= |mid_C1:149| |C1':153|)
        |                       (= |mid_D100:147| |D100':154|)
        |                       (= 0 |K:150|)
        |                       (= (+ |K:146| |K:150|) |K:155|)
        |                       (<= 0 |K:155|)
        |                       (<= 0 |R':152|)
        |                       (<= 0 |D100':154|)
        |                       (<= 0 |C1':153|)
        |                       (<= 0 |i':151|)
        |                       (= (+ (- 0 |i':151|) |R':152|) 0)
        |                       (= (+ (- 0 |i':151|) |C1':153|) 0)
        |                       (< |i':151| |param0:12|)
        |                       (= D100 1)
        |                       (= C1 (+ |C1':153| 1))
        |                       (= R |R':152|)
        |                       (= i (+ |i':151| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (j Int)
        |                    (i Int)
        |                    (R Int)
        |                    (|mid_j:211| Int)
        |                    (|j':217| Int)
        |                    (|K:219| Int)
        |                    (|param0:14| Int)
        |                    (|param1:17| Int)
        |                    (|K:222| Int)
        |                    (|mid_R:223| Int)
        |                    (|mid_C1:212| Int)
        |                    (|D100':226| Int)
        |                    (|K:228| Int)
        |                    (|mid_D100:221| Int)
        |                    (|j':225| Int)
        |                    (|i':215| Int)
        |                    (|K:214| Int)
        |                    (|D100':216| Int)
        |                    (|R':213| Int)
        |                    (|K:224| Int)
        |                    (|C1':218| Int)
        |                    (|mid_i:207| Int)
        |                    (|mid_D100:210| Int)
        |                    (|mid_R:209| Int)
        |                    (|R':227| Int)
        |                    (|mid_j:220| Int)
        |                    (|j:3| Int)
        |                    (|K:208| Int))
        |             (! (let ((a!1 (= |mid_j:211| (* (ite (= |K:208| 0) 1 0) |j:3|)))
        |                      (a!2 (and (<= 1 |K:208|)
        |                                (<= 0 (+ (- 0 1) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (= (+ (- 0 |mid_C1:212|) |mid_i:207|) 0)
        |                                (= |mid_j:211| 0)
        |                                (= |mid_D100:210| 0)
        |                                (<= 0 (+ (- 0 |mid_C1:212|) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (<= 0 |mid_R:209|)
        |                                (<= 0 (+ (- 0 1) |mid_C1:212|))))
        |                      (a!3 (not (<= (+ (- 0 |param1:17|) 1) 0)))
        |                      (a!4 (+ (* (ite (= |K:214| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:214| 0) 1 0) |mid_D100:210|)))
        |                      (a!5 (+ (* (ite (= |K:214| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:214| 0) 1 0) |mid_j:211|)))
        |                      (a!6 (= (+ |i':215| (- 0 |C1':218|))
        |                              (* (ite (= |K:214| 0) 1 0)
        |                                 (+ |mid_i:207| (- 0 |mid_C1:212|)))))
        |                      (a!7 (and (<= 1 |K:214|)
        |                                (= (+ (- 0 |mid_i:207|) |mid_C1:212|) 0)
        |                                (<= 0 (+ (- 0 1) (- 0 |mid_i:207|) |param0:14|))
        |                                (<= 0 |mid_R:209|)
        |                                (<= 0 (+ (- 0 1) |param1:17|))
        |                                (<= 0 |mid_D100:210|)
        |                                (<= 0 |mid_i:207|)
        |                                (= (+ (- 0 |C1':218|) |i':215|) 0)
        |                                (= (+ (- 0 |j':217|) |D100':216|) 0)
        |                                (= (+ (- 0 |j':217|) |param1:17|) 0)
        |                                (<= 0 (+ (- 0 |C1':218|) |param0:14|))
        |                                (<= 0 (+ (- 0 |j':217|) |R':213|))
        |                                (<= 0 (+ (- 0 1) |j':217|))
        |                                (<= 0 (+ (- 0 1) |C1':218|))))
        |                      (a!8 (or (= 0 |K:214|) (<= (+ (- 0 |param1:17|) 1) 0)))
        |                      (a!9 (and (<= 1 |K:222|)
        |                                (<= 0 (+ (- 0 1) |param1:17|))
        |                                (<= 0 |R':213|)
        |                                (= (+ (- 0 |mid_D100:221|) |mid_j:220|) 0)
        |                                (<= 0 (+ (- 0 |mid_D100:221|) |param1:17|))
        |                                (<= 0 (+ (- 0 1) |mid_R:223|))
        |                                (<= 0 (+ (- 0 1) |mid_D100:221|)))))
        |                  (and (= |mid_i:207| |K:208|)
        |                       (= |mid_R:209| 0)
        |                       (= |mid_D100:210| 0)
        |                       a!1
        |                       (= (+ (- 0 |mid_i:207|) |mid_C1:212|) 0)
        |                       (<= |mid_D100:210| 0)
        |                       (= |K:208| |K:208|)
        |                       (or (and (= |K:208| 0)
        |                                (= |j:3| |mid_j:211|)
        |                                (= 0 |mid_i:207|)
        |                                (= 0 |mid_D100:210|)
        |                                (= 0 |mid_C1:212|)
        |                                (= 0 |mid_R:209|))
        |                           a!2)
        |                       (or (= 0 |K:208|) a!3)
        |                       (= |R':213| (+ (* |K:214| |param1:17|) |mid_R:209|))
        |                       (= |i':215| (+ |K:214| |mid_i:207|))
        |                       (= |D100':216| a!4)
        |                       (= |j':217| a!5)
        |                       a!6
        |                       (<= |D100':216|
        |                           (+ (* |K:214| |param1:17|) |mid_D100:210|))
        |                       (= |K:214| |K:214|)
        |                       (or (and (= |K:214| 0)
        |                                (= |mid_j:211| |j':217|)
        |                                (= |mid_i:207| |i':215|)
        |                                (= |mid_D100:210| |D100':216|)
        |                                (= |mid_C1:212| |C1':218|)
        |                                (= |mid_R:209| |R':213|))
        |                           a!7)
        |                       a!8
        |                       (= (+ |K:208| |K:214|) |K:219|)
        |                       (<= 0 |K:219|)
        |                       (<= 0 |D100':216|)
        |                       (<= 0 |R':213|)
        |                       (<= 0 |C1':218|)
        |                       (<= 0 |i':215|)
        |                       (= (+ (- 0 |i':215|) |C1':218|) 0)
        |                       (< |i':215| |param0:14|)
        |                       (= (+ (- 0 |mid_j:220|) |mid_D100:221|) 0)
        |                       (= |mid_j:220| |K:222|)
        |                       (= |mid_R:223| (+ |K:222| |R':213|))
        |                       (= |K:222| |K:222|)
        |                       (or (and (= |K:222| 0)
        |                                (= 0 |mid_j:220|)
        |                                (= 0 |mid_D100:221|)
        |                                (= |R':213| |mid_R:223|))
        |                           a!9)
        |                       (= |K:224| |K:224|)
        |                       (= |K:224| 0)
        |                       (= |mid_j:220| |j':225|)
        |                       (= |mid_D100:221| |D100':226|)
        |                       (= |mid_R:223| |R':227|)
        |                       (= 0 |K:224|)
        |                       (= (+ |K:222| |K:224|) |K:228|)
        |                       (<= 0 |K:228|)
        |                       (<= 0 |D100':226|)
        |                       (<= 0 |R':227|)
        |                       (<= 0 |j':225|)
        |                       (= (+ (- 0 |j':225|) |D100':226|) 0)
        |                       (< |j':225| |param1:17|)
        |                       (= R (+ |R':227| 1))
        |                       (= C1 (+ |C1':218| 1))
        |                       (= D100 (+ |D100':226| 1))
        |                       (= i |i':215|)
        |                       (= j (+ |j':225| 1))
        |                       (= n |param0:14|)
        |                       (= m |param1:17|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", BoundCheckingUnitTest.test01, test01Expected),
      TestCaseJavaProgram("Test02", BoundCheckingUnitTest.test02, test02Expected),
    )
  }

  val deltaVariableResetTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (j Int)
        |                    (i Int)
        |                    (R Int)
        |                    (|C1':154| Int)
        |                    (|D100':155| Int)
        |                    (|K:151| Int)
        |                    (|i':152| Int)
        |                    (|param0:12| Int)
        |                    (|R':153| Int)
        |                    (|K:156| Int)
        |                    (|mid_i:146| Int)
        |                    (|K:147| Int)
        |                    (|mid_C1:150| Int)
        |                    (|mid_R:149| Int)
        |                    (|mid_D100:148| Int))
        |             (! (let ((a!1 (and (<= 1 |K:147|)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |mid_R:149|) |mid_C1:150|) 0)
        |                                (= (+ (- 0 |mid_R:149|) |mid_i:146|) 0)
        |                                (= (+ (- 0 1) |mid_D100:148|) 0)
        |                                (<= 0 (+ (- 0 |mid_R:149|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |mid_R:149|)))))
        |                  (and (= |mid_i:146| |K:147|)
        |                       (= |mid_D100:148| (ite (= |K:147| 0) 0 1))
        |                       (= (+ |mid_i:146| (- 0 |mid_R:149|)) 0)
        |                       (= (+ |mid_C1:150| (- 0 |mid_R:149|)) 0)
        |                       (<= |mid_D100:148| |K:147|)
        |                       (= |K:147| |K:147|)
        |                       (or (and (= |K:147| 0)
        |                                (= 0 |mid_i:146|)
        |                                (= 0 |mid_R:149|)
        |                                (= 0 |mid_C1:150|)
        |                                (= 0 |mid_D100:148|))
        |                           a!1)
        |                       (= |K:151| |K:151|)
        |                       (= |K:151| 0)
        |                       (= |mid_i:146| |i':152|)
        |                       (= |mid_R:149| |R':153|)
        |                       (= |mid_C1:150| |C1':154|)
        |                       (= |mid_D100:148| |D100':155|)
        |                       (= 0 |K:151|)
        |                       (= (+ |K:147| |K:151|) |K:156|)
        |                       (<= 0 |K:156|)
        |                       (<= 0 |R':153|)
        |                       (<= 0 |D100':155|)
        |                       (<= 0 |C1':154|)
        |                       (<= 0 |i':152|)
        |                       (= (+ (- 0 |i':152|) |R':153|) 0)
        |                       (= (+ (- 0 |i':152|) |C1':154|) 0)
        |                       (< |i':152| |param0:12|)
        |                       (= D100 |D100':155|)
        |                       (= C1 (+ |C1':154| 1))
        |                       (= R |R':153|)
        |                       (= i (+ |i':152| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((C1 Int)
        |                    (j Int)
        |                    (i Int)
        |                    (R Int)
        |                    (|mid_j:211| Int)
        |                    (|j':217| Int)
        |                    (|K:219| Int)
        |                    (|param0:14| Int)
        |                    (|param1:17| Int)
        |                    (|mid_C1:212| Int)
        |                    (|i':215| Int)
        |                    (|K:214| Int)
        |                    (|D100':216| Int)
        |                    (|R':213| Int)
        |                    (|C1':218| Int)
        |                    (|mid_i:207| Int)
        |                    (|mid_D100:210| Int)
        |                    (|j:4| Int)
        |                    (|mid_R:209| Int)
        |                    (|K:208| Int))
        |             (! (let ((a!1 (= |mid_j:211| (* (ite (= |K:208| 0) 1 0) |j:4|)))
        |                      (a!2 (and (<= 1 |K:208|)
        |                                (<= 0 (+ (- 0 1) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (= (+ (- 0 |mid_C1:212|) |mid_i:207|) 0)
        |                                (= |mid_j:211| 0)
        |                                (= |mid_D100:210| 0)
        |                                (<= 0 (+ (- 0 |mid_C1:212|) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (<= 0 |mid_R:209|)
        |                                (<= 0 (+ (- 0 1) |mid_C1:212|))))
        |                      (a!3 (not (<= (+ (- 0 |param1:17|) 1) 0)))
        |                      (a!4 (+ (* (ite (= |K:214| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:214| 0) 1 0) |mid_D100:210|)))
        |                      (a!5 (+ (* (ite (= |K:214| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:214| 0) 1 0) |mid_j:211|)))
        |                      (a!6 (= (+ |i':215| (- 0 |C1':218|))
        |                              (* (ite (= |K:214| 0) 1 0)
        |                                 (+ |mid_i:207| (- 0 |mid_C1:212|)))))
        |                      (a!7 (and (<= 1 |K:214|)
        |                                (= (+ (- 0 |mid_i:207|) |mid_C1:212|) 0)
        |                                (<= 0 (+ (- 0 1) (- 0 |mid_i:207|) |param0:14|))
        |                                (<= 0 |mid_R:209|)
        |                                (<= 0 (+ (- 0 1) |param1:17|))
        |                                (<= 0 |mid_D100:210|)
        |                                (<= 0 |mid_i:207|)
        |                                (= (+ (- 0 |C1':218|) |i':215|) 0)
        |                                (= (+ (- 0 |j':217|) |D100':216|) 0)
        |                                (= (+ (- 0 |j':217|) |param1:17|) 0)
        |                                (<= 0 (+ (- 0 |C1':218|) |param0:14|))
        |                                (<= 0 (+ (- 0 |j':217|) |R':213|))
        |                                (<= 0 (+ (- 0 1) |j':217|))
        |                                (<= 0 (+ (- 0 1) |C1':218|))))
        |                      (a!8 (or (= 0 |K:214|) (<= (+ (- 0 |param1:17|) 1) 0))))
        |                  (and (= |mid_i:207| |K:208|)
        |                       (= |mid_R:209| 0)
        |                       (= |mid_D100:210| 0)
        |                       a!1
        |                       (= (+ (- 0 |mid_i:207|) |mid_C1:212|) 0)
        |                       (<= |mid_D100:210| 0)
        |                       (= |K:208| |K:208|)
        |                       (or (and (= |K:208| 0)
        |                                (= |j:4| |mid_j:211|)
        |                                (= 0 |mid_i:207|)
        |                                (= 0 |mid_D100:210|)
        |                                (= 0 |mid_C1:212|)
        |                                (= 0 |mid_R:209|))
        |                           a!2)
        |                       (or (= 0 |K:208|) a!3)
        |                       (= |R':213| (+ (* |K:214| |param1:17|) |mid_R:209|))
        |                       (= |i':215| (+ |K:214| |mid_i:207|))
        |                       (= |D100':216| a!4)
        |                       (= |j':217| a!5)
        |                       a!6
        |                       (<= |D100':216|
        |                           (+ (* |K:214| |param1:17|) |mid_D100:210|))
        |                       (= |K:214| |K:214|)
        |                       (or (and (= |K:214| 0)
        |                                (= |mid_j:211| |j':217|)
        |                                (= |mid_i:207| |i':215|)
        |                                (= |mid_D100:210| |D100':216|)
        |                                (= |mid_C1:212| |C1':218|)
        |                                (= |mid_R:209| |R':213|))
        |                           a!7)
        |                       a!8
        |                       (= (+ |K:208| |K:214|) |K:219|)
        |                       (<= 0 |K:219|)
        |                       (<= 0 |R':213|)
        |                       (<= 0 |D100':216|)
        |                       (<= 0 |C1':218|)
        |                       (<= 0 |i':215|)
        |                       (= (+ (- 0 |i':215|) |C1':218|) 0)
        |                       (< |i':215| |param0:14|)
        |                       (= R |R':213|)
        |                       (= C1 (+ |C1':218| 1))
        |                       (= D100 |D100':216|)
        |                       (= i |i':215|)
        |                       (= j 0)
        |                       (= n |param0:14|)
        |                       (= m |param1:17|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", BoundCheckingUnitTest.test01, test01Expected),
      TestCaseJavaProgram("Test02", BoundCheckingUnitTest.test02, test02Expected),
    )
  }

  val counterVariableUpdateTests: HashSet[TestCaseJavaProgram] = {
    val test01Expected =
      """(let ((a!1 (exists ((j Int)
        |                    (i Int)
        |                    (D100 Int)
        |                    (R Int)
        |                    (|C1':154| Int)
        |                    (|D100':155| Int)
        |                    (|K:151| Int)
        |                    (|i':152| Int)
        |                    (|param0:12| Int)
        |                    (|R':153| Int)
        |                    (|K:156| Int)
        |                    (|mid_i:146| Int)
        |                    (|K:147| Int)
        |                    (|mid_C1:150| Int)
        |                    (|mid_R:149| Int)
        |                    (|mid_D100:148| Int))
        |             (! (let ((a!1 (and (<= 1 |K:147|)
        |                                (<= 0 (+ (- 0 1) |param0:12|))
        |                                (= (+ (- 0 |mid_R:149|) |mid_C1:150|) 0)
        |                                (= (+ (- 0 |mid_R:149|) |mid_i:146|) 0)
        |                                (= (+ (- 0 1) |mid_D100:148|) 0)
        |                                (<= 0 (+ (- 0 |mid_R:149|) |param0:12|))
        |                                (<= 0 (+ (- 0 1) |mid_R:149|)))))
        |                  (and (= |mid_i:146| |K:147|)
        |                       (= |mid_D100:148| (ite (= |K:147| 0) 0 1))
        |                       (= (+ |mid_i:146| (- 0 |mid_R:149|)) 0)
        |                       (= (+ |mid_C1:150| (- 0 |mid_R:149|)) 0)
        |                       (<= |mid_D100:148| |K:147|)
        |                       (= |K:147| |K:147|)
        |                       (or (and (= |K:147| 0)
        |                                (= 0 |mid_i:146|)
        |                                (= 0 |mid_R:149|)
        |                                (= 0 |mid_C1:150|)
        |                                (= 0 |mid_D100:148|))
        |                           a!1)
        |                       (= |K:151| |K:151|)
        |                       (= |K:151| 0)
        |                       (= |mid_i:146| |i':152|)
        |                       (= |mid_R:149| |R':153|)
        |                       (= |mid_C1:150| |C1':154|)
        |                       (= |mid_D100:148| |D100':155|)
        |                       (= 0 |K:151|)
        |                       (= (+ |K:147| |K:151|) |K:156|)
        |                       (<= 0 |K:156|)
        |                       (<= 0 |R':153|)
        |                       (<= 0 |D100':155|)
        |                       (<= 0 |C1':154|)
        |                       (<= 0 |i':152|)
        |                       (= (+ (- 0 |i':152|) |R':153|) 0)
        |                       (= (+ (- 0 |i':152|) |C1':154|) 0)
        |                       (< |i':152| |param0:12|)
        |                       (= D100 |D100':155|)
        |                       (= C1 (+ |C1':154| 1))
        |                       (= R |R':153|)
        |                       (= i (+ |i':152| 1))
        |                       (= n |param0:12|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin
    val test02Expected =
      """(let ((a!1 (exists ((j Int)
        |                    (i Int)
        |                    (D100 Int)
        |                    (R Int)
        |                    (|mid_j:211| Int)
        |                    (|j':217| Int)
        |                    (|K:219| Int)
        |                    (|param0:14| Int)
        |                    (|param1:17| Int)
        |                    (|mid_C1:212| Int)
        |                    (|i':215| Int)
        |                    (|K:214| Int)
        |                    (|D100':216| Int)
        |                    (|R':213| Int)
        |                    (|C1':218| Int)
        |                    (|mid_i:207| Int)
        |                    (|mid_D100:210| Int)
        |                    (|mid_R:209| Int)
        |                    (|j:3| Int)
        |                    (|K:208| Int))
        |             (! (let ((a!1 (= |mid_j:211| (* (ite (= |K:208| 0) 1 0) |j:3|)))
        |                      (a!2 (and (<= 1 |K:208|)
        |                                (<= 0 (+ (- 0 1) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (= (+ (- 0 |mid_C1:212|) |mid_i:207|) 0)
        |                                (= |mid_j:211| 0)
        |                                (= |mid_D100:210| 0)
        |                                (<= 0 (+ (- 0 |mid_C1:212|) |param0:14|))
        |                                (<= 0 (- 0 |param1:17|))
        |                                (<= 0 |mid_R:209|)
        |                                (<= 0 (+ (- 0 1) |mid_C1:212|))))
        |                      (a!3 (not (<= (+ (- 0 |param1:17|) 1) 0)))
        |                      (a!4 (+ (* (ite (= |K:214| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:214| 0) 1 0) |mid_D100:210|)))
        |                      (a!5 (+ (* (ite (= |K:214| 0) 0 1) |param1:17|)
        |                              (* (ite (= |K:214| 0) 1 0) |mid_j:211|)))
        |                      (a!6 (= (+ |i':215| (- 0 |C1':218|))
        |                              (* (ite (= |K:214| 0) 1 0)
        |                                 (+ |mid_i:207| (- 0 |mid_C1:212|)))))
        |                      (a!7 (and (<= 1 |K:214|)
        |                                (= (+ (- 0 |mid_i:207|) |mid_C1:212|) 0)
        |                                (<= 0 (+ (- 0 1) (- 0 |mid_i:207|) |param0:14|))
        |                                (<= 0 |mid_R:209|)
        |                                (<= 0 (+ (- 0 1) |param1:17|))
        |                                (<= 0 |mid_D100:210|)
        |                                (<= 0 |mid_i:207|)
        |                                (= (+ (- 0 |C1':218|) |i':215|) 0)
        |                                (= (+ (- 0 |j':217|) |D100':216|) 0)
        |                                (= (+ (- 0 |j':217|) |param1:17|) 0)
        |                                (<= 0 (+ (- 0 |C1':218|) |param0:14|))
        |                                (<= 0 (+ (- 0 |j':217|) |R':213|))
        |                                (<= 0 (+ (- 0 1) |j':217|))
        |                                (<= 0 (+ (- 0 1) |C1':218|))))
        |                      (a!8 (or (= 0 |K:214|) (<= (+ (- 0 |param1:17|) 1) 0))))
        |                  (and (= |mid_i:207| |K:208|)
        |                       (= |mid_R:209| 0)
        |                       (= |mid_D100:210| 0)
        |                       a!1
        |                       (= (+ (- 0 |mid_i:207|) |mid_C1:212|) 0)
        |                       (<= |mid_D100:210| 0)
        |                       (= |K:208| |K:208|)
        |                       (or (and (= |K:208| 0)
        |                                (= |j:3| |mid_j:211|)
        |                                (= 0 |mid_i:207|)
        |                                (= 0 |mid_D100:210|)
        |                                (= 0 |mid_C1:212|)
        |                                (= 0 |mid_R:209|))
        |                           a!2)
        |                       (or (= 0 |K:208|) a!3)
        |                       (= |R':213| (+ (* |K:214| |param1:17|) |mid_R:209|))
        |                       (= |i':215| (+ |K:214| |mid_i:207|))
        |                       (= |D100':216| a!4)
        |                       (= |j':217| a!5)
        |                       a!6
        |                       (<= |D100':216|
        |                           (+ (* |K:214| |param1:17|) |mid_D100:210|))
        |                       (= |K:214| |K:214|)
        |                       (or (and (= |K:214| 0)
        |                                (= |mid_j:211| |j':217|)
        |                                (= |mid_i:207| |i':215|)
        |                                (= |mid_D100:210| |D100':216|)
        |                                (= |mid_C1:212| |C1':218|)
        |                                (= |mid_R:209| |R':213|))
        |                           a!7)
        |                       a!8
        |                       (= (+ |K:208| |K:214|) |K:219|)
        |                       (<= 0 |K:219|)
        |                       (<= 0 |D100':216|)
        |                       (<= 0 |R':213|)
        |                       (<= 0 |C1':218|)
        |                       (<= 0 |i':215|)
        |                       (= (+ (- 0 |i':215|) |C1':218|) 0)
        |                       (< |i':215| |param0:14|)
        |                       (= R |R':213|)
        |                       (= C1 (+ |C1':218| 1))
        |                       (= D100 |D100':216|)
        |                       (= i |i':215|)
        |                       (= j 0)
        |                       (= n |param0:14|)
        |                       (= m |param1:17|)))
        |                :weight 0))))
        |  (or a!1))""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", BoundCheckingUnitTest.test01, test01Expected),
      TestCaseJavaProgram("Test02", BoundCheckingUnitTest.test02, test02Expected),
    )
  }
}