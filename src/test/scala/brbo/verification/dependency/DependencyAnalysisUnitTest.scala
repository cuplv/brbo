package brbo.verification.dependency

import brbo.{StringCompare, TestCaseJavaProgram}
import brbo.common.CommandLineArguments.DEFAULT_ARGUMENTS
import brbo.verification.BasicProcessor
import brbo.verification.decomposition.{DecompositionUnitTest, TaintSet}
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

class DependencyAnalysisUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[DependencyAnalysisUnitTest])

  "Taint set computation" should "be correct" in {
    DependencyAnalysisUnitTest.taintSetTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val result = DependencyAnalysis.controlDataDependencyForResources(targetMethod, DEFAULT_ARGUMENTS.getDebugMode).toTestString
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Transitive data dependency analysis" should "be correct" in {
    DependencyAnalysisUnitTest.dataDependencyTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val reachingDefinitions = targetMethod.reachingDefinitions
        val result = targetMethod.commandsNodesMap.map({
          case (command, nodes) =>
            val taintSets = nodes.map(node => DependencyAnalysis.transitiveDataDependency(node.node, reachingDefinitions, Set(), excludeResourceVariables = true, debug = false))
            s"$command -> ${TaintSet.merge(taintSets).toTestString}"
        }).toList.sorted
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }
}

object DependencyAnalysisUnitTest {
  private val test01: String =
    """class Test01 {
      |  void f(int n, int m, int l)
      |  {
      |    int a = 0;
      |    int R = 0;
      |    a = a + 1;
      |    R = R + a;
      |  }
      |}""".stripMargin

      private val test02: String =
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

      private val test03: String =
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

  private val test04: String =
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

  private val test05: String =
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

  val taintSetTests: List[TestCaseJavaProgram] = {
    val test01ExpectedOutput =
      """All: List(R, a). Inputs: List()""".stripMargin

    val test02ExpectedOutput =
      """All: List(R, a, n). Inputs: List(n)""".stripMargin

    val test03ExpectedOutput =
      """All: List(R, n, p, q, x). Inputs: List(n)""".stripMargin

    val test04ExpectedOutput =
      """All: List(R, i, j, n). Inputs: List(n)""".stripMargin

    val test05ExpectedOutput =
      """All: List(R, a, b, i, n). Inputs: List(n)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
      TestCaseJavaProgram("Test04", test04, test04ExpectedOutput),
      TestCaseJavaProgram("Test05", test05, test05ExpectedOutput),
    )
  }

  val dataDependencyTests: List[TestCaseJavaProgram] = {
    val test01ExpectedOutput =
      """R = R + a; -> All: List(R, a). Inputs: List()
        |a = a + 1; -> All: List(a). Inputs: List()
        |int R = 0 -> All: List(). Inputs: List()
        |int a = 0 -> All: List(). Inputs: List()""".stripMargin

    val test02ExpectedOutput =
      """R = R + a; -> All: List(R, a). Inputs: List()
        |int R = 0 -> All: List(). Inputs: List()
        |int a = 0 -> All: List(). Inputs: List()""".stripMargin

    val test03ExpectedOutput =
      """R = R + 1; -> All: List(R). Inputs: List()
        |int R = 0 -> All: List(). Inputs: List()
        |int p = q -> All: List(q). Inputs: List()
        |int q = 0 -> All: List(). Inputs: List()
        |int x = n -> All: List(n). Inputs: List(n)
        |p--; -> All: List(p, q). Inputs: List()
        |q = 0; -> All: List(). Inputs: List()
        |q++; -> All: List(q). Inputs: List()
        |x--; -> All: List(n, x). Inputs: List(n)""".stripMargin

    val test04ExpectedOutput =
      """R = R + 1; -> All: List(R). Inputs: List()
        |i++; -> All: List(i). Inputs: List()
        |int R = 0 -> All: List(). Inputs: List()
        |int i = 0 -> All: List(). Inputs: List()
        |int j = i + 1 -> All: List(i). Inputs: List()
        |j++; -> All: List(i, j). Inputs: List()
        |j++; -> All: List(j). Inputs: List()
        |n--; -> All: List(n). Inputs: List(n)""".stripMargin

    val test05ExpectedOutput =
      """R = R + 1; -> All: List(R). Inputs: List()
        |a--; -> All: List(a). Inputs: List()
        |a--; -> All: List(a, n). Inputs: List(n)
        |b++; -> All: List(b). Inputs: List()
        |b++; -> All: List(b). Inputs: List()
        |b--; -> All: List(b). Inputs: List()
        |i++; -> All: List(i, n). Inputs: List(n)
        |int R = 0 -> All: List(). Inputs: List()
        |int a = n -> All: List(n). Inputs: List(n)
        |int b = 0 -> All: List(). Inputs: List()
        |int i = n - 1 -> All: List(n). Inputs: List(n)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
      TestCaseJavaProgram("Test04", test04, test04ExpectedOutput),
      TestCaseJavaProgram("Test05", test05, test05ExpectedOutput),
    )
  }
}
