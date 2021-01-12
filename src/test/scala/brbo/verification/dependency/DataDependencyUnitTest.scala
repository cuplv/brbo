package brbo.verification.dependency

import brbo.{StringCompare, TestCaseJavaProgram}
import brbo.verification.BasicProcessor
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashSet

class DataDependencyUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[DataDependencyUnitTest])

  "Data dependency analysis" should "produce correct results" in {
    DataDependencyUnitTest.dataDependencyUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        assert(StringCompare.ignoreWhitespaces(DataDependency.computeDependencySet(targetMethod).toString(), testCase.expectedOutput))
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
      """Set()""".stripMargin

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
      """Set()""".stripMargin

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
      """Set(n)""".stripMargin

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
      """Set(n)""".stripMargin

    val test05: String =
      """class Test04 {
        |  void f(int n, int m, int l)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    a = a + n;
        |    R = R + a;
        |  }
        |}""".stripMargin
    val test05ExpectedOutput =
      """Set(n)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
      TestCaseJavaProgram("Test04", test04, test04ExpectedOutput),
    )
  }
}