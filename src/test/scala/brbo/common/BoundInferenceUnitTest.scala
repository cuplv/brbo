package brbo.common

import brbo.common.BeforeOrAfterOrThis.AFTER
import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.verification.BasicProcessor
import brbo.{StringCompare, TestCase, TestCaseJavaProgram}
import com.sun.source.tree.ExpressionStatementTree
import org.scalatest.flatspec.AnyFlatSpec

class BoundInferenceUnitTest extends AnyFlatSpec {
  "Generating template polynomials" should "succeed" in {
    BoundInferenceUnitTest.generateTemplatePolynomialUnitTest.foreach({
      testCase =>
        val inputVariables = List[String]("x", "y", "z")
        val result = BoundInference.generateTemplateInvariant(testCase.input.toInt, 100, inputVariables)
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, s"${testCase.name} failed"))
    })
  }

  "Inferring bounds" should "be correct" in {
    BoundInferenceUnitTest.inferBoundUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val boundInference = new BoundInference(targetMethod)
        val solver = new Z3Solver
        val result = boundInference.inferBound(
          solver,
          Locations(
            {
              case expressionStatementTree: ExpressionStatementTree =>
                GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource) match {
                  case Some(_) => true
                  case None => false
                }
              case _ => false
            },
            AFTER
          ),
          "R"
        )
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, s"${testCase.className} failed"))
    })
  }
}

object BoundInferenceUnitTest {
  val generateTemplatePolynomialUnitTest: List[TestCase] = {
    val test01Expected =
      """(100 * 1 * 1 * 1) + (100 * 1 * 1 * 1 * z) + (100 * 1 * x * 1 * 1) + (100 * 1 * 1 * y * 1)""".stripMargin

    val test02Expected =
      """(100 * 1 * 1 * 1) + (100 * 1 * 1 * 1 * z) + (100 * 1 * x * 1 * 1) + (100 * 1 * 1 * y * 1) + (100 * 1 * x * x * 1 * 1) + (100 * 1 * 1 * 1 * z * z) + (100 * 1 * x * 1 * 1 * z) + (100 * 1 * 1 * y * y * 1) + (100 * 1 * x * 1 * y * 1) + (100 * 1 * 1 * y * 1 * z)""".stripMargin

    val test03Expected =
      """(100 * 1 * 1 * 1) + (100 * 1 * 1 * 1 * z) + (100 * 1 * x * 1 * 1) + (100 * 1 * 1 * y * 1) + (100 * 1 * x * x * 1 * 1) + (100 * 1 * 1 * 1 * z * z) + (100 * 1 * x * 1 * 1 * z) + (100 * 1 * 1 * y * y * 1) + (100 * 1 * x * 1 * y * 1) + (100 * 1 * 1 * y * 1 * z) + (100 * 1 * x * x * 1 * 1 * z) + (100 * 1 * 1 * 1 * z * z * z) + (100 * 1 * x * 1 * 1 * z * z) + (100 * 1 * 1 * y * y * 1 * z) + (100 * 1 * x * 1 * y * 1 * z) + (100 * 1 * 1 * y * y * y * 1) + (100 * 1 * x * 1 * y * y * 1) + (100 * 1 * x * x * x * 1 * 1) + (100 * 1 * 1 * y * 1 * z * z) + (100 * 1 * x * x * 1 * y * 1)""".stripMargin

    List[TestCase](
      TestCase("Test01", "1", test01Expected),
      TestCase("Test02", "2", test02Expected),
      TestCase("Test03", "3", test03Expected),
    )
  }

  val inferBoundUnitTest: List[TestCaseJavaProgram] = {
    val test01: String = // A loop with a nesting depth of 1
      """class Test01 {
        |  void f(int n)
        |  {
        |    int R = 0;
        |    int i = 0;
        |    if (n >= 0) {
        |     while (i < n)
        |     {
        |       i++;
        |       R = R + 1;
        |     }
        |    }
        |  }
        |}""".stripMargin
    val test01Expected =
      """(+ 0 (* 1 1 n))""".stripMargin

    val test02: String = // A loop with a nesting depth of 2
      """class Test02 {
        |  void f(int n, int m)
        |  {
        |    int R = 0;
        |    int i = 0;
        |    if (n >= 0 && m >= 0) {
        |     while (i < n) {
        |       int j = 0;
        |       while (j < m) {
        |         j++;
        |         R = R + 1;
        |       }
        |       i++;
        |     }
        |    }
        |  }
        |}""".stripMargin
    val test02Expected =
      """(let ((a!1 (* 100 (* (* (* 1 m) m) m)))
        |      (a!2 (* 100 (* (* (* 1 n) n) m)))
        |      (a!3 (* 100 (* (* (* 1 n) n) n)))
        |      (a!4 (* 100 (* (* (* 1 n) m) m)))
        |      (a!5 (* 100 (* (* (* 1 m) m) m) m))
        |      (a!6 (* 100 (* (* (* 1 n) n) m) m))
        |      (a!7 (* 100 (* (* (* 1 n) n) n) n))
        |      (a!8 (* 100 (* (* (* 1 n) n) n) m))
        |      (a!9 (* 100 (* (* (* 1 n) m) m) m)))
        |  (+ 0
        |     (* 100 1)
        |     (* 100 (* 1 m))
        |     (* 100 (* 1 n))
        |     (* 100 (* (* 1 m) m))
        |     (* 100 (* (* 1 n) n))
        |     (* 100 (* (* 1 n) m))
        |     a!1
        |     a!2
        |     a!3
        |     a!4
        |     a!5
        |     a!6
        |     a!7
        |     a!8
        |     a!9))""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01Expected),
      TestCaseJavaProgram("Test02", test02, test02Expected)
    )
  }
}