package brbo.verification.dependency

import brbo.verification.BasicProcessor
import brbo.{StringCompare, TestCaseJavaProgram}
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

class ControlDependencyUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[ControlDependencyUnitTest])

  "Control dependency analysis" should "be correct" in {
    ControlDependencyUnitTest.controlDependencyUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val result = {
          val result = ControlDependency.computeControlDependency(targetMethod)
          ControlDependency.reverseControlDependency(result).map({
            case (block, blocks) => (block, blocks.toList.sortWith(_.getUid < _.getUid))
          }).toList.sortWith(_._1.getUid < _._1.getUid)
        }
        val resultString = result.map({
          case (block, blocks) => s"${block.getUid} -> ${blocks.map(b => b.getUid)}"
        }).mkString("\n")
        // To debug,
        // 1. Manually modify *.dot files s.t. each block's label include its uid
        // 2. Run `dot -Tpdf X.dot -o X.pdf`
        // import brbo.common.CFGUtils
        // CFGUtils.printPDF(targetMethod.cfg)
        assert(StringCompare.ignoreWhitespaces(resultString, testCase.expectedOutput, testCase.className))
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
      """191 -> List()
        |193 -> List()
        |194 -> List()""".stripMargin

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
      """196 -> List()
        |198 -> List()
        |199 -> List()
        |200 -> List()
        |204 -> List(200)""".stripMargin

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
      """208 -> List()
        |210 -> List()
        |211 -> List()
        |212 -> List(213)
        |213 -> List()
        |217 -> List(213)
        |218 -> List(213)
        |222 -> List(218)
        |223 -> List(218, 224)
        |224 -> List(218)
        |228 -> List(224)
        |230 -> List(218)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
    )
  }
}