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
        StringCompare.ignoreWhitespaces(resultString, testCase.expectedOutput, testCase.className)
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
    val test01ExpectedOutput = {
      val start = 957
      s"""$start -> List()
         |${start + 2} -> List()
         |${start + 3} -> List()""".stripMargin
    }

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
    val test02ExpectedOutput = {
      val start = 962
      s"""$start -> List()
         |${start + 2} -> List()
         |${start + 3} -> List()
         |${start + 4} -> List()
         |${start + 8} -> List(${start + 4})""".stripMargin
    }

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
    val test03ExpectedOutput = {
      val start = 974
      s"""$start -> List()
         |${start + 2} -> List()
         |${start + 3} -> List()
         |${start + 4} -> List(${start + 5})
         |${start + 5} -> List()
         |${start + 9} -> List(${start + 5})
         |${start + 10} -> List(${start + 5})
         |${start + 14} -> List(${start + 10})
         |${start + 15} -> List(${start + 10}, ${start + 16})
         |${start + 16} -> List(${start + 10})
         |${start + 20} -> List(${start + 16})
         |${start + 22} -> List(${start + 10})""".stripMargin
    }

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
    )
  }
}