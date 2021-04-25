package brbo.verification.decomposition

import brbo.{StringCompare, TestCaseJavaProgram}
import brbo.common.CommandLineArguments.DEFAULT_ARGUMENTS
import brbo.verification.BasicProcessor
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

class NewDecompositionUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[NewDecompositionUnitTest])

  "Merging groups" should "be correct" in {
    NewDecompositionUnitTest.mergeTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new NewDecomposition(targetMethod, DEFAULT_ARGUMENTS)
        val groups = decomposition.initializeGroups()
        val newGroups = decomposition.mergeGroups(groups)
        val string = newGroups.elements.toList.map({
          group => s"Group(${group.resetLocation.toString}, ${group.updates.toList.map(u => u.toString).sorted})"
        }).sorted
        assert(StringCompare.ignoreWhitespaces(string, testCase.expectedOutput, testCase.className))
    })
  }

  "Deciding reset locations" should "be correct" in {

  }
}

object NewDecompositionUnitTest {
  val mergeTests: List[TestCaseJavaProgram] = {
    val test01: String =
      """class Test01 {
        |  void f(int n, int m, int l)
        |  {
        |    int R = 0;
        |    R = R + 1;
        |    R = R + m;
        |  }
        |}""".stripMargin
    val test01ExpectedOutput =
      """Group(None, List(Update(R = R + 1;,R = (R + 1))))
        |Group(None, List(Update(R = R + m;,R = (R + m))))""".stripMargin

    val test02: String =
      """class Test02 {
        |  void f(int n, int m, int l)
        |  {
        |    int R = 0;
        |    int a = 0;
        |    a = a + 1;
        |    R = R + 1;
        |    R = R + a;
        |  }
        |}""".stripMargin
    val test02ExpectedOutput =
      """Group(None, List(Update(R = R + 1;,R = (R + 1))))
        |Group(None, List(Update(R = R + a;,R = (R + a))))""".stripMargin

    val test03: String =
      """class Test03 {
        |  void f(int n, int m, int l)
        |  {
        |    int R = 0;
        |    int a = m;
        |    R = R + 1;
        |    R = R + a;
        |    R = R + m;
        |  }
        |}""".stripMargin
    val test03ExpectedOutput =
      """Group(None, List(Update(R = R + 1;,R = (R + 1))))
        |Group(None, List(Update(R = R + a;,R = (R + a))))
        |Group(None, List(Update(R = R + m;,R = (R + m))))""".stripMargin

    val test04: String =
      """class Test04 {
        |  void f(int n, int m, int l)
        |  {
        |    int R = 0;
        |    int a = m;
        |    int b = a;
        |    R = R + 1;
        |    R = R + a;
        |    R = R + b;
        |    R = R + (a + 1);
        |  }
        |}""".stripMargin
    val test04ExpectedOutput =
      """Group(None, List(Update(R = R + (a + 1);,R = (R + (a + 1))), Update(R = R + a;,R = (R + a)), Update(R = R + b;,R = (R + b))))
        |Group(None, List(Update(R = R + 1;,R = (R + 1))))""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
      TestCaseJavaProgram("Test04", test04, test04ExpectedOutput),
    )
  }
}
