package brbo.verification

import brbo.common.MathUtils
import brbo.{StringCompare, TestCaseJavaProgram}
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

class DecompositionUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[DecompositionUnitTest])

  "Initializing subprograms" should "be correct" in {
    DecompositionUnitTest.initializeSubprogramsUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod)
        val result = decomposition.initializeSubprograms()
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Merging subprograms" should "be correct" in {
    DecompositionUnitTest.mergeSubprogramsUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod)
        val initialSubprograms: Set[decomposition.Subprogram] = decomposition.initializeSubprograms()
        val result: Traversable[decomposition.Subprogram] = MathUtils.crossJoin(List(initialSubprograms, initialSubprograms)).map({
          subprograms =>
            val subprogram1 = subprograms.head
            val subprogram2 = subprograms.tail.head
            val mergeResult = decomposition.merge(subprogram1, subprogram2)
            logger.debug(s"$subprogram1 --- $subprogram2 --- $mergeResult")
            mergeResult
        }).toSet
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Merging subprograms if they overlap" should "be correct" in {
    DecompositionUnitTest.mergeIfOverlapSubprogramsUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod)
        val result = decomposition.mergeIfOverlap(decomposition.initializeSubprograms())
        assert(StringCompare.ignoreWhitespaces(result.programs, testCase.expectedOutput, testCase.className))
    })
  }

  "Enlarging subprograms" should "be correct" in {
    DecompositionUnitTest.enlargeSubprogramsUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod)
        val initialSubprograms: Set[decomposition.Subprogram] = decomposition.initializeSubprograms()
        val result: Set[decomposition.Subprogram] = initialSubprograms.map({
          subprogram =>
            val enlargeResult = decomposition.enlarge(subprogram)
            logger.debug(enlargeResult)
            enlargeResult
        })
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Taint set computation" should "be correct" in {
    DecompositionUnitTest.taintSetTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val result = Decomposition.computeTaintSet(targetMethod, debug = false)
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Computing environment modified set" should "be correct" in {
    DecompositionUnitTest.environmentModifiedSetUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod)
        val subprograms = decomposition.mergeIfOverlap(decomposition.initializeSubprograms())
        val result = decomposition.environmentModifiedSet(subprograms.programs.head, subprograms)
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Deciding subsequent execution" should "be correct" in {
    DecompositionUnitTest.subsequentExecutionUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod)
        val initialSubprograms: List[decomposition.Subprogram] = decomposition.initializeSubprograms().toList
        val result = MathUtils.crossJoin2(initialSubprograms, initialSubprograms).map({
          case (subprogram1, subprogram2) =>
            logger.debug(s"Subprogram 1: $subprogram1")
            logger.debug(s"Subprogram 2: $subprogram2")
            decomposition.subsequentExecution(subprogram1, subprogram2)
        })
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Decomposition" should "succeed" in {
    DecompositionUnitTest.decompositionUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod)
        val result = decomposition.decompose().programs
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Inserting resets and counters" should "be correct" in {
    DecompositionUnitTest.insertingResetsAndUpdatesUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod)
        val subprograms = decomposition.mergeIfOverlap(decomposition.initializeSubprograms())
        val result = decomposition.insertGhostVariables(subprograms)
        assert(StringCompare.ignoreWhitespaces(result.sourceFileContents, testCase.expectedOutput, testCase.className))
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
      """""".stripMargin

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
      """""".stripMargin

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
      """n""".stripMargin

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
      """n""".stripMargin

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
        |      int j = 0;
        |      while (j < m) {
        |        j++;
        |        R = R + 1;
        |      }
        |      i++;
        |    }
        |  }
        |}""".stripMargin
    val test01ExpectedOutput =
      """Subprogram(
        |int j = 0
        |while (j < m) {
        |    j++;
        |    R = R + 1;
        |}
        |)""".stripMargin

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
        |        start = end + text; // ndInt(end + 1, text);
        |        if (start == text) break;
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
    val test02ExpectedOutput =
      """Subprogram(
        |R = R + separator;
        |)
        |Subprogram(
        |int index = 0
        |int start = 0
        |int end = 0
        |while (true) {
        |    start = end + text;
        |    if (start == text) break;
        |    end = start + text;
        |    sb += start - index;
        |    R = R + (start - index);
        |    index = end;
        |}
        |sb += text - index;
        |R = R + (text - index);
        |)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
    )
  }

  val initializeSubprogramsUnitTest: List[TestCaseJavaProgram] = {
    val test01 =
      """class Test01 {
        |  void f(int n, int m)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    R = R + 1;
        |    R = R + a;
        |  }
        |}""".stripMargin
    val test01ExpectedOutput =
      """Subprogram(
        |R = R + 1;
        |)
        |Subprogram(
        |R = R + a;
        |)""".stripMargin

    val test02 =
      """class Test02 {
        |  void f(int n, int m)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    while (a < n) {
        |      if (a > m) {
        |        R = R + 1;
        |      }
        |    }
        |
        |    for (int i = 0; i < m; i++) {
        |      if (a < n) {
        |        R = R + 2;
        |      }
        |    }
        |  }
        |}""".stripMargin
    val test02ExpectedOutput =
      """Subprogram(
        |for (int i = 0; i < m; i++) {
        |    if (a < n) {
        |        R = R + 2;
        |    }
        |}
        |)
        |Subprogram(
        |while (a < n) {
        |    if (a > m) {
        |        R = R + 1;
        |    }
        |}
        |)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
    )
  }

  val mergeSubprogramsUnitTest: List[TestCaseJavaProgram] = {
    val test01 =
      """class Test01 {
        |  void f(int n, int m)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    R = R + 1;
        |    a = a + 2;
        |    R = R + a;
        |  }
        |}""".stripMargin
    val test01ExpectedOutput =
      """Subprogram(
        |R = R + 1;
        |)
        |Subprogram(
        |R = R + 1;
        |a = a + 2;
        |R = R + a;
        |)
        |Subprogram(
        |R = R + a;
        |)""".stripMargin

    val test02 =
      """class Test02 {
        |  void f(int n, int m)
        |  {
        |    int b = 0;
        |    int R = 0;
        |    while (b < n) {
        |      if (b > m) {
        |        R = R + 1;
        |      }
        |    }
        |    b = b + 3;
        |    R = R + b;
        |  }
        |}""".stripMargin
    val test02ExpectedOutput =
      """Subprogram(
        |R = R + b;
        |)
        |Subprogram(
        |while (b < n) {
        |    if (b > m) {
        |        R = R + 1;
        |    }
        |}
        |)
        |Subprogram(
        |while (b < n) {
        |    if (b > m) {
        |        R = R + 1;
        |    }
        |}
        |b = b + 3;
        |R = R + b;
        |)""".stripMargin

    val test03 =
      """class Test03 {
        |  void f(int n, int m)
        |  {
        |    int c = 0;
        |    int R = 0;
        |    while (c < n) {
        |      if (c > m) {
        |        R = R + c;
        |      }
        |    }
        |    c = c + 5;
        |    R = R + c;
        |  }
        |}""".stripMargin
    val test03ExpectedOutput =
      """Subprogram(
        |R = R + c;
        |)
        |Subprogram(
        |while (c < n) {
        |    if (c > m) {
        |        R = R + c;
        |    }
        |}
        |c = c + 5;
        |R = R + c;
        |)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
    )
  }

  val mergeIfOverlapSubprogramsUnitTest: List[TestCaseJavaProgram] = {
    val test01 =
      """class Test01 {
        |  void f(int n, int m)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    R = R + 1;
        |    a = a + 2;
        |    R = R + a;
        |  }
        |}""".stripMargin
    val test01ExpectedOutput =
      """Subprogram(
        |R = R + 1;
        |)
        |Subprogram(
        |R = R + a;
        |)""".stripMargin

    val test02 =
      """class Test02 {
        |  void f(int n, int m)
        |  {
        |    int b = 0;
        |    int R = 0;
        |    while (b < n) {
        |      if (b > m) {
        |        R = R + 1;
        |        R = R + b;
        |      }
        |    }
        |  }
        |}""".stripMargin
    val test02ExpectedOutput =
      """Subprogram(
        |while (b < n) {
        |    if (b > m) {
        |        R = R + 1;
        |        R = R + b;
        |    }
        |}
        |)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
    )
  }

  val enlargeSubprogramsUnitTest: List[TestCaseJavaProgram] = {
    val test01 =
      """class Test01 {
        |  void f(int n, int m)
        |  {
        |    int a = 0;
        |    int R = 0;
        |    {
        |      R = R + a;
        |    }
        |  }
        |}""".stripMargin
    val test01ExpectedOutput =
      """Subprogram(
        |{
        |    R = R + a;
        |}
        |)""".stripMargin

    val test02 =
      """class Test02 {
        |  void f(int n, int m)
        |  {
        |    int b = 0;
        |    int R = 0;
        |    {
        |      b = b + 2;
        |      R = R + b;
        |    }
        |  }
        |}""".stripMargin
    val test02ExpectedOutput =
      """Subprogram(
        |b = b + 2;
        |R = R + b;
        |)""".stripMargin

    val test03 =
      """class Test03 {
        |  void f(int n, int m)
        |  {
        |    int c = 0;
        |    int R = 0;
        |    {
        |      R = R + c;
        |      c = c + 3;
        |    }
        |  }
        |}""".stripMargin
    val test03ExpectedOutput =
      """Subprogram(
        |R = R + c;
        |c = c + 3;
        |)""".stripMargin

    val test04 =
      """class Test04 {
        |  void f(int n, int m)
        |  {
        |    int d = 0;
        |    int R = 0;
        |    if (d < n)
        |      R = R + d;
        |  }
        |}""".stripMargin
    val test04ExpectedOutput =
      """Subprogram(
        |if (d < n) R = R + d;
        |)""".stripMargin

    val test05 =
      """class Test05 {
        |  void f(int n, int m)
        |  {
        |    int e = 0;
        |    int R = 0;
        |    if (e < n)
        |      R = R + e;
        |    else
        |      e = e + 2;
        |  }
        |}""".stripMargin
    val test05ExpectedOutput =
      """Subprogram(
        |if (e < n) R = R + e; else e = e + 2;
        |)""".stripMargin

    val test06 =
      """class Test06 {
        |  void f(int n, int m)
        |  {
        |    int f = 0;
        |    int R = 0;
        |    for (int i = 0; i < n; i++)
        |      R = R + f;
        |  }
        |}""".stripMargin
    val test06ExpectedOutput =
      """Subprogram(
        |for (int i = 0; i < n; i++) R = R + f;
        |)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
      TestCaseJavaProgram("Test04", test04, test04ExpectedOutput),
      TestCaseJavaProgram("Test05", test05, test05ExpectedOutput),
      TestCaseJavaProgram("Test06", test06, test06ExpectedOutput),
    )
  }

  val environmentModifiedSetUnitTest: List[TestCaseJavaProgram] = {
    val test01 =
      """class Test01 {
        |  void f(int n, int m) {
        |    int it = n;
        |    int R = 0;
        |    while (it > 0) {
        |      it--;
        |      R = R + m;
        |    }
        |  }
        |}""".stripMargin
    val test01ExpectedOutput = "it"

    val test02 =
      """class Test02 {
        |  void f(int n, int m) {
        |    int it = n;
        |    int R = 0;
        |    it++;
        |    while (it > 0) {
        |      it--;
        |      R = R + 1;
        |    }
        |  }
        |}""".stripMargin
    val test02ExpectedOutput = ""

    val test03 =
      """class Test03 {
        |  void f(int n, int m, int l) {
        |    int it = n;
        |    int R = 0;
        |    while (it > 0) {
        |      it--;
        |      int it2 = m;
        |      while (it2 > 0) {
        |        it2--;
        |        R = R + 1;
        |      }
        |    }
        |  }
        |}""".stripMargin
    val test03ExpectedOutput = "it\nit2"

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
      TestCaseJavaProgram("Test03", test03, test03ExpectedOutput),
    )
  }

  val subsequentExecutionUnitTest: List[TestCaseJavaProgram] = {
    val test01 =
      """class Test01 {
        |  void f(int n, int m) {
        |    int it = n;
        |    int R = 0;
        |    while (it > 0) {
        |      it--;
        |      R = R + m;
        |    }
        |    R = R + n;
        |  }
        |}""".stripMargin
    val test01ExpectedOutput = "false\nfalse\ntrue\ntrue"

    val test02 =
      """class Test02 {
        |  void f(int n, int m) {
        |    int it = n;
        |    int R = 0;
        |    it++;
        |    while (it > 0) {
        |      it--;
        |      R = R + m;
        |      R = R + n;
        |    }
        |  }
        |}""".stripMargin
    val test02ExpectedOutput = "true\ntrue\ntrue\ntrue"

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
    )
  }

  val insertingResetsAndUpdatesUnitTest: List[TestCaseJavaProgram] = {
    val test01 =
      """class Test01 {
        |  void f(int n, int m) {
        |    int it = n;
        |    int R = 0;
        |    while (it > 0) {
        |      it--;
        |      R = R + m;
        |    }
        |    R = R + n;
        |  }
        |}""".stripMargin
    val test01ExpectedOutput =
      """import brbo.benchmarks.Common;
        |abstract class Test01 extends Common {
        |  void f(int C6, int n, int C7, int D1, int m, int D0)
        |  {
        |    int it = n;
        |    int R = 0;
        |    while (it > 0)
        |    {
        |      it--;;
        |      D0 = 0; C6 = C6 + 1;;D0 = D0 + m;
        |      R = R + m;;
        |    }
        |    D1 = 0; C7 = C7 + 1;;D1 = D1 + n;
        |    R = R + n;;
        |  }
        |}""".stripMargin

    val test02 =
      """class Test02 {
        |  void f(int n, int m) {
        |    int it = n;
        |    int R = 0;
        |    it++;
        |    while (it > 0) {
        |      it--;
        |      R = R + 1;
        |      R = R + n;
        |    }
        |  }
        |}""".stripMargin
    val test02ExpectedOutput =
      """import brbo.benchmarks.Common;
        |abstract class Test02 extends Common {
        |  void f(int n, int m, int C4, int D0)
        |  {
        |    int it = n;
        |    int R = 0;
        |    it++;;
        |    D0 = 0; C4 = C4 + 1;;
        |    while (it > 0)
        |    {
        |      it--;;
        |      D0 = D0 + 1;
        |      R = R + 1;;
        |      D0 = D0 + n;
        |      R = R + n;;
        |    }
        |  }
        |}""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
    )
  }
}
