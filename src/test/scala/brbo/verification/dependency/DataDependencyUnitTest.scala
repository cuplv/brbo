package brbo.verification.dependency

import brbo.verification.BasicProcessor
import brbo.{StringCompare, TestCaseJavaProgram}
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

class DataDependencyUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[DataDependencyUnitTest])

  "Data dependency analysis" should "produce correct results" in {
    DataDependencyUnitTest.dataDependencyUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val dataDependency = DataDependency.computeDataDependency(targetMethod)
        val string = dataDependency.toList.map({
          case (node, values) =>
            s"${node.toString} -> (${values.toList.sortWith({ case (v1, v2) => v1.toString < v2.toString }).mkString(", ")})"
        }).sorted
        assert(StringCompare.ignoreWhitespaces(string, testCase.expectedOutput, testCase.className))
    })
  }
}

object DataDependencyUnitTest {
  val dataDependencyUnitTest: List[TestCaseJavaProgram] = {
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
      """(R + a) -> (`R` in `R = 0`, `a` in `a = (a + 1)`, `l` (input), `m` (input), `n` (input))
        |(a + 1) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |0 -> (`R` in `R`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |1 -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R -> (`R` in `R = 0`, `a` in `a = (a + 1)`, `l` (input), `m` (input), `n` (input))
        |R -> (`a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = (R + a) -> (`R` in `R = 0`, `a` in `a = (a + 1)`, `l` (input), `m` (input), `n` (input))
        |R = 0 -> (`R` in `R`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a -> (`R` in `R = 0`, `a` in `a = (a + 1)`, `l` (input), `m` (input), `n` (input))
        |a -> (`l` (input), `m` (input), `n` (input))
        |a = (a + 1) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a = 0 -> (`a` in `a`, `l` (input), `m` (input), `n` (input))""".stripMargin

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
      """(R + a) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |(a < n) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |0 -> (`R` in `R`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R -> (`a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = (R + a) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = 0 -> (`R` in `R`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a -> (`l` (input), `m` (input), `n` (input))
        |a = 0 -> (`a` in `a`, `l` (input), `m` (input), `n` (input))
        |n -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))""".stripMargin

    val test03: String =
      """class Test03 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    if (n > 0)
        |      a = a + n;
        |    R = R + a;
        |  }
        |}""".stripMargin
    val test03ExpectedOutput =
      """(R + a) -> (`R` in `R = 0`, `a` in `a = (a + n)`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |(a + n) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |(n > 0) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |0 -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R -> (`R` in `R = 0`, `a` in `a = (a + n)`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R -> (`a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = (R + a) -> (`R` in `R = 0`, `a` in `a = (a + n)`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = 0 -> (`R` in `R`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a -> (`R` in `R = 0`, `a` in `a = (a + n)`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a -> (`l` (input), `m` (input), `n` (input))
        |a = (a + n) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a = 0 -> (`a` in `a`, `l` (input), `m` (input), `n` (input))
        |n -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))""".stripMargin

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
      """(R + a) -> (`R` in `R = 0`, `a` in `a = (a + n)`, `l` (input), `m` (input), `n` (input))
        |(a + n) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |0 -> (`R` in `R`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R -> (`R` in `R = 0`, `a` in `a = (a + n)`, `l` (input), `m` (input), `n` (input))
        |R -> (`a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |R = (R + a) -> (`R` in `R = 0`, `a` in `a = (a + n)`, `l` (input), `m` (input), `n` (input))
        |R = 0 -> (`R` in `R`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a -> (`R` in `R = 0`, `a` in `a = (a + n)`, `l` (input), `m` (input), `n` (input))
        |a -> (`l` (input), `m` (input), `n` (input))
        |a = (a + n) -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))
        |a = 0 -> (`a` in `a`, `l` (input), `m` (input), `n` (input))
        |n -> (`R` in `R = 0`, `a` in `a = 0`, `l` (input), `m` (input), `n` (input))""".stripMargin

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
      """""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
      TestCaseJavaProgram("Test04", test04, test04ExpectedOutput),
      // TestCaseJavaProgram("Test05", test05, test05ExpectedOutput),
    )
  }
}