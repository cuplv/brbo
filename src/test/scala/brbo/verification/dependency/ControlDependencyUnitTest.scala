package brbo.verification.dependency

import brbo.verification.BasicProcessor
import brbo.verification.dependency.ControlDependencyUnitTest.controlDependencyUnitTest
import brbo.{StringCompare, TestCaseJavaProgram}
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

class ControlDependencyUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[ControlDependencyUnitTest])

  "Control dependency analysis" should "be correct" in {
    controlDependencyUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val result = ControlDependency.computeControlDependency(targetMethod).map({
          case (block, blocks) => (block, blocks.toList.sortWith(_.getUid < _.getUid))
        }).toList.sortWith(_._1.getUid < _._1.getUid)
        val resultString = result.map({
          case (block, blocks) => s"${block.getUid} -> ${blocks.map(b => b.getUid)}"
        }).mkString("\n")
        // To debug,
        // 1. Manually modify *.dot files s.t. each block's label include its uid
        // 2. Run `dot -Tpdf X.dot -o X.pdf`
        // CFGUtils.printPDF(targetMethod.cfg)
        assert(StringCompare.ignoreWhitespaces(resultString, testCase.expectedOutput))
    })
  }
}

object ControlDependencyUnitTest {
  val controlDependencyUnitTest: List[TestCaseJavaProgram] = {
    val test01: String =
      """class Test01 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    a = a + 1;
        |    R = R + a;
        |  }
        |}""".stripMargin
    val test01ExpectedOutput =
      """""".stripMargin

    val test02: String =
      """class Test02 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    if (a < n)
        |      R = R + a;
        |  }
        |}""".stripMargin
    val test02ExpectedOutput =
      """231 -> List(231)""".stripMargin

    val test03: String =
      """class Test03 {
        |  void f(int n, int m, int l)
        |  {
        |    int q = 0;
        |    int x = n;
        |    int R = 0;
        |    while (x > 0) {
        |      x--;
        |      q++;
        |      if (q > 0) {
        |        int p = q;
        |        while (p > 0) {
        |          p--;
        |          R = R + 1;
        |        }
        |        q = 0;
        |      }
        |    }
        |  }
        |}""".stripMargin
    val test03ExpectedOutput =
      """239 -> List(239)
        |240 -> List(240)
        |244 -> List(240, 244)
        |245 -> List(240, 245)
        |249 -> List(240, 245, 249)
        |250 -> List(250)
        |251 -> List(240, 245, 251)
        |255 -> List(240, 245, 251, 255)
        |257 -> List(240, 245, 257)""".stripMargin

    val test04: String =
      """class Test04 {
        |  void f(int n, int m, int l)
        |  {
        |    int i = 0;
        |    int R = 0;
        |    while (i < n) {
        |      int j = i + 1;
        |      while (j < n) {
        |        R = R + 1;
        |        j++;
        |        n--;
        |        j++;
        |      }
        |      i++;
        |    }
        |  }
        |}""".stripMargin
    val test04ExpectedOutput =
      """Set(n)""".stripMargin

    val test05: String =
      """class Test05 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = 0;
        |    int b = 0;
        |    int R = 0;
        |    while (a > 0) {
        |      a--;
        |      b++;
        |      while (b > 0) {
        |        b--;
        |        int i = n - 1;
        |        while (i > 0) {
        |          if (a > 0) {
        |            R = R + 1;
        |            a--;
        |            b++;
        |          }
        |          i++;
        |        }
        |      }
        |    }
        |  }
        |}""".stripMargin
    val test05ExpectedOutput =
      """Set()""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
      // TestCaseJavaProgram("Test04", test04, test04ExpectedOutput),
      // TestCaseJavaProgram("Test05", test05, test05ExpectedOutput),
    )
  }
}