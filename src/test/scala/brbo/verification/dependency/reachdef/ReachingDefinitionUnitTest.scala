package brbo.verification.dependency.reachdef

import brbo.verification.BasicProcessor
import brbo.verification.dependency.ReachingDefinition
import brbo.{StringCompare, TestCaseJavaProgram}
import com.sun.source.tree.ExpressionStatementTree
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.JavaConverters._

class ReachingDefinitionUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[ReachingDefinitionUnitTest])

  "Reaching definition analysis" should "be correct" in {
    ReachingDefinitionUnitTest.reachingDefinitionUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val result = ReachingDefinition.run(targetMethod)
        val output = targetMethod.cfg.getAllNodes.asScala.toList.map({
          node =>
            val r: String = {
              val definitions = result.get(node) match {
                case Some(definitions) => definitions.toList.sortWith({ case (v1, v2) => v1.toString < v2.toString }).mkString(", ")
                case None => ""
              }
              val care = targetMethod.sortedCommands.exists({
                command =>
                  if (command == node.getTree) true
                  else {
                    command match {
                      case e: ExpressionStatementTree => e.getExpression == node.getTree
                      case _ => false
                    }
                  }
              })
              if (care) definitions
              else "Don't care"
            }
            s"$node (`${node.getTree}`) -> ($r)"
        }).sorted
        assert(StringCompare.ignoreWhitespaces(output, testCase.expectedOutput, testCase.className))
    })
  }
}

object ReachingDefinitionUnitTest {
  val reachingDefinitionUnitTest: List[TestCaseJavaProgram] = {
    val test01: String =
      """class Test01 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = 2;
        |    int R = 0;
        |    a = a + 1;
        |    R = R + a;
        |  }
        |}""".stripMargin
    val test01ExpectedOutput =
      """(R + a) (`R + a`) -> (Don't care)
        |(a + 1) (`a + 1`) -> (Don't care)
        |0 (`0`) -> (Don't care)
        |1 (`1`) -> (Don't care)
        |2 (`2`) -> (Don't care)
        |R (`R`) -> (Don't care)
        |R (`R`) -> (Don't care)
        |R (`int R = 0`) -> (`a` in `a = 2`, `l` (input), `m` (input), `n` (input))
        |R = (R + a) (`R = R + a`) -> (`R` in `R = 0`, `a` in `a = (a + 1)`, `l` (input), `m` (input), `n` (input))
        |R = 0 (`int R = 0`) -> (`a` in `a = 2`, `l` (input), `m` (input), `n` (input))
        |a (`a`) -> (Don't care)
        |a (`a`) -> (Don't care)
        |a (`a`) -> (Don't care)
        |a (`int a = 2`) -> (`l` (input), `m` (input), `n` (input))
        |a = (a + 1) (`a = a + 1`) -> (`R` in `R = 0`, `a` in `a = 2`, `l` (input), `m` (input), `n` (input))
        |a = 2 (`int a = 2`) -> (`l` (input), `m` (input), `n` (input))""".stripMargin

    val test02: String =
      """class Test02 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    if (a < n) {
        |      R = R + a;
        |    }
        |  }
        |}""".stripMargin
    val test02ExpectedOutput =
      """(R + a) (`R + a`) -> (Don't care)
        |(a < n) (`a < n`) -> (Don't care)
        |0 (`0`) -> (Don't care)
        |0 (`0`) -> (Don't care)
        |R (`R`) -> (Don't care)
        |R (`R`) -> (Don't care)
        |R (`int R = 0`) -> (`a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = (R + a) (`R = R + a`) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = 0 (`int R = 0`) -> (`a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a (`a`) -> (Don't care)
        |a (`a`) -> (Don't care)
        |a (`int a = 0`) -> (`l` (input), `m` (input), `n` (input))
        |a = 0 (`int a = 0`) -> (`l` (input), `m` (input), `n` (input))
        |n (`n`) -> (Don't care)""".stripMargin

    val test03: String =
      """class Test03 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    while (n > 0) {
        |      a = a + n;
        |    }
        |    R = R + a;
        |  }
        |}""".stripMargin
    val test03ExpectedOutput =
      """(R + a) (`R + a`) -> (Don't care)
        |(a + n) (`a + n`) -> (Don't care)
        |(n > 0) (`n > 0`) -> (Don't care)
        |0 (`0`) -> (Don't care)
        |0 (`0`) -> (Don't care)
        |0 (`0`) -> (Don't care)
        |R (`R`) -> (Don't care)
        |R (`R`) -> (Don't care)
        |R (`int R = 0`) -> (`a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = (R + a) (`R = R + a`) -> (`R` in `R = 0`, `a` in `a = (a + n)`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = 0 (`int R = 0`) -> (`a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a (`a`) -> (Don't care)
        |a (`a`) -> (Don't care)
        |a (`a`) -> (Don't care)
        |a (`int a = 0`) -> (`l` (input), `m` (input), `n` (input))
        |a = (a + n) (`a = a + n`) -> (`R` in `R = 0`, `a` in `a = (a + n)`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a = 0 (`int a = 0`) -> (`l` (input), `m` (input), `n` (input))
        |n (`n`) -> (Don't care)
        |n (`n`) -> (Don't care)""".stripMargin

    val test04: String =
      """class Test04 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    a = a + n;
        |    R = R + a;
        |  }
        |}""".stripMargin
    val test04ExpectedOutput =
      """(R + a) (`R + a`) -> (Don't care)
        |(a + n) (`a + n`) -> (Don't care)
        |0 (`0`) -> (Don't care)
        |0 (`0`) -> (Don't care)
        |R (`R`) -> (Don't care)
        |R (`R`) -> (Don't care)
        |R (`int R = 0`) -> (`a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = (R + a) (`R = R + a`) -> (`R` in `R = 0`, `a` in `a = (a + n)`, `l` (input), `m` (input), `n` (input))
        |R = 0 (`int R = 0`) -> (`a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a (`a`) -> (Don't care)
        |a (`a`) -> (Don't care)
        |a (`a`) -> (Don't care)
        |a (`int a = 0`) -> (`l` (input), `m` (input), `n` (input))
        |a = (a + n) (`a = a + n`) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a = 0 (`int a = 0`) -> (`l` (input), `m` (input), `n` (input))
        |n (`n`) -> (Don't care)""".stripMargin

    val test05: String =
      """class Test05 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = 0;
        |    int b = 0;
        |    int R = 0;
        |    while (a > 0) {
        |      a = a - 1;
        |      b = b + 1;
        |      while (b > 0) {
        |        b = b - 1;
        |        int i = n - 1;
        |        while (i > 0) {
        |          if (a > 0) {
        |            R = R + 1;
        |            a = a - 1;
        |            b = b + 1;
        |          }
        |          i++;
        |        }
        |      }
        |    }
        |  }
        |}""".stripMargin
    val test05ExpectedOutput =
      """""".stripMargin

    val test06: String = // A loop with a nesting depth of 1
      """class Test06 {
        |  void f(int n)
        |  {
        |    int D100 = 0;
        |    int C1 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      i = i + 1;
        |      C1 = C1 + 1;
        |      D100 = 0;
        |      D100 = D100 + 1;
        |      R = R + 1;
        |    }
        |  }
        |}""".stripMargin

    val test06ExpectedOutput =
      """""".stripMargin

    val test07: String =
      """class Test07 {
        |  void f(int R, int end, int i, int index, int n, int sb, int separator, int start, int templateds, int text) {
        |     {
        |        R = R + (text - index);
        |        sb += separator;
        |        i = i + 1;
        |        {
        |            while (i < templateds) {
        |                index = 0;
        |                start = 0;
        |                end = 0;
        |                while (n > 0) {
        |                    start = end + text;
        |                    end = start + text;
        |                    sb += start - index;
        |                    R = R + (start - index);
        |                    index = end;
        |                }
        |                if (true) { return; }
        |                R = R + (text - index);
        |                sb += separator;
        |            }
        |            {
        |                if (true) { return; }
        |            }
        |        }
        |    }
        |  }
        |}""".stripMargin
    val test07ExpectedOutput =
      """""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
      TestCaseJavaProgram("Test04", test04, test04ExpectedOutput),
      // TestCaseJavaProgram("Test05", test05, test05ExpectedOutput),
      // TestCaseJavaProgram("Test06", test06, test06ExpectedOutput),
      // TestCaseJavaProgram("Test07", test07, test07ExpectedOutput),
    )
  }
}