package brbo.verification.decomposition

import brbo.common.CommandLineArguments.DEFAULT_ARGUMENTS
import brbo.verification.BasicProcessor
import brbo.{StringCompare, TestCaseJavaProgram}
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

class NewDecompositionUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[NewDecompositionUnitTest])

  "Merging groups" should "be correct" in {
    NewDecompositionUnitTest.mergeTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new NewDecomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val groups = decomposition.initializeGroups()
        val newGroups = decomposition.mergeGroups(groups)
        val string = newGroups.elements.toList.map({ group => group.toTestString }).sorted
        assert(StringCompare.ignoreWhitespaces(string, testCase.expectedOutput, testCase.className))
    })
  }

  "Selective amortization" should "be correct" in {
    NewDecompositionUnitTest.decomposeSelectiveAmortizationUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new NewDecomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val groups = decomposition.selectiveAmortize.groups
        val string = groups.toTestString
        assert(StringCompare.ignoreWhitespaces(string, testCase.expectedOutput, testCase.className))
    })
  }

  "No amortization" should "be correct" in {
    NewDecompositionUnitTest.decomposeNoAmortizationUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new NewDecomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val groups = decomposition.noAmortize.groups
        val string = groups.toTestString
        assert(StringCompare.ignoreWhitespaces(string, testCase.expectedOutput, testCase.className))
    })
  }

  "Full amortization" should "be correct" in {
    NewDecompositionUnitTest.decomposeFullAmortizationUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new NewDecomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val groups = decomposition.fullAmortize.groups
        val string = groups.toTestString
        assert(StringCompare.ignoreWhitespaces(string, testCase.expectedOutput, testCase.className))
    })
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
        |Group(None, List(Update(R = R + a;,R = (R + a)), Update(R = R + m;,R = (R + m))))""".stripMargin

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

  private val decompositionTest01 =
    """class Test01 {
      |  void f(int n, int m) {
      |    int R = 0;
      |    int i = 0;
      |    while (i < n) {
      |      int j = 0;
      |      while (j < m) {
      |        j++;
      |        R = R + 1;
      |      }
      |      i++;
      |    }
      |  }
      |}""".stripMargin

  private val decompositionTest02 =
    """class Test02 {
      |  void f(int text, int templateds, int separator, int n) {
      |    int R = 0;
      |    int sb = 0;
      |    int i = 0;
      |    while (i < templateds) {
      |      int index = 0;
      |      int start = 0;
      |      int end = 0;
      |      while (n > 0) {
      |        start = end + text; // ndInt(end + 1, text);
      |        end = start + text; // ndInt(start + 1, text);
      |        sb += start - index;
      |        R = R + (start - index);
      |        index = end;
      |      }
      |      sb += text - index;
      |      R = R + (text - index);
      |      sb += separator;
      |      R = R + separator;
      |      i++;
      |    }
      |  }
      |}""".stripMargin

  val decomposeSelectiveAmortizationUnitTest: List[TestCaseJavaProgram] = {
    val test01ExpectedOutput =
      """Group(Some(R = R + 1;), List(Update(R = R + 1;,R = (R + 1))))""".stripMargin

    val test02ExpectedOutput =
      """Group(Some(R = R + separator;), List(Update(R = R + separator;,R = (R + separator))))
        |Group(Some(int sb = 0), List(Update(R = R + (start - index);,R = (R + (start - index))), Update(R = R + (text - index);,R = (R + (text - index)))))""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", decompositionTest01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", decompositionTest02, test02ExpectedOutput),
    )
  }

  val decomposeNoAmortizationUnitTest: List[TestCaseJavaProgram] = {
    val test01ExpectedOutput =
      """Group(Some(R = R + 1;), List(Update(R = R + 1;,R = (R + 1))))""".stripMargin

    val test02ExpectedOutput =
      """Group(Some(R = R + (start - index);), List(Update(R = R + (start - index);,R = (R + (start - index)))))
        |Group(Some(R = R + (text - index);), List(Update(R = R + (text - index);,R = (R + (text - index)))))
        |Group(Some(R = R + separator;), List(Update(R = R + separator;,R = (R + separator))))""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", decompositionTest01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", decompositionTest02, test02ExpectedOutput),
    )
  }

  val decomposeFullAmortizationUnitTest: List[TestCaseJavaProgram] = {
    val test01ExpectedOutput =
      """Group(Some(int R = 0), List(Update(R = R + 1;,R = (R + 1))))""".stripMargin

    val test02ExpectedOutput =
      """Group(Some(int R = 0), List(Update(R = R + (start - index);,R = (R + (start - index))), Update(R = R + (text - index);,R = (R + (text - index))), Update(R = R + separator;,R = (R + separator))))""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", decompositionTest01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", decompositionTest02, test02ExpectedOutput),
    )
  }
}
