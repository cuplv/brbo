package brbo.verification

import brbo.{StringCompare, TestCaseJavaProgram}
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

class DecompositionUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[CounterAxiomGeneratorUnitTest])

  "Taint set computation" should "be correct" in {
    DecompositionUnitTest.taintSetTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val result = Decomposition.computeTaintSetControlAndData(targetMethod).toList.sortWith(_ < _)
        assert(StringCompare.ignoreWhitespaces(result.toString(), testCase.expectedOutput))
    })
  }
}

object DecompositionUnitTest {
  val taintSetTests: List[TestCaseJavaProgram] = {
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
      """List(R, a)""".stripMargin

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
      """List(R, a, n)""".stripMargin

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
      """List(R, n, p, q, x)""".stripMargin

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
      """List(R, i, j, n)""".stripMargin

    val test05: String =
      """class Test05 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = n;
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
      """List(R, a, b, i, n)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
      TestCaseJavaProgram("Test04", test04, test04ExpectedOutput),
      TestCaseJavaProgram("Test05", test05, test05ExpectedOutput),
    )
  }

  val decompositionUnitTest: List[TestCaseJavaProgram] = {
    val test01 =
      """class Test01 {
        |  void f(int n, int m) {
        |    int R = 0;
        |    int i = 0;
        |    while (i < n) {
        |      int j;
        |      j = 0;
        |      while (j < m) {
        |        j++;
        |        R = R + 1;
        |      }
        |      i++;
        |    }
        |  }
        |}""".stripMargin
    val test01ExpectedOutput = ""
    val test02 =
      """class Test02 {
        |  void f(int text, int templateds, int separator) {
        |    int R = 0;
        |    int sb = 0;
        |    int i = 0;
        |    while (i < templateds) {
        |      int index = 0;
        |      int start = 0;
        |      int end = 0;
        |      while (true) {
        |        start = ndInt(end + 1, text);
        |        if (start == text) break;
        |        end = ndInt(start + 1, text);
        |        sb += start - index;
        |        R = R + start - index;
        |        index = end;
        |      }
        |      sb += text - index;
        |      R = R + text - index;
        |      sb += separator;
        |      R = R + separator;
        |      i++;
        |    }
        |  }
        |}""".stripMargin
    val test02ExpectedOutput = ""

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
    )
  }
}
