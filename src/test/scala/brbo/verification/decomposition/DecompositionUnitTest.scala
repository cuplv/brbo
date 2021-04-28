package brbo.verification.decomposition

import brbo.common.CommandLineArguments.DEFAULT_ARGUMENTS
import brbo.common.MathUtils
import brbo.verification.AmortizationMode.UNKNOWN
import brbo.verification.BasicProcessor
import brbo.verification.dependency.DependencyAnalysis
import brbo.{StringCompare, TestCaseJavaProgram}
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

class DecompositionUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[DecompositionUnitTest])

  "Initializing subprograms" should "be correct" in {
    DecompositionUnitTest.initializeSubprogramsUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val result = decomposition.initializeSubprograms()
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Merging subprograms" should "be correct" in {
    DecompositionUnitTest.mergeSubprogramsUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val initialSubprograms: Set[Subprogram] = decomposition.initializeSubprograms()
        val result: Traversable[Subprogram] = MathUtils.crossJoin(List(initialSubprograms, initialSubprograms)).map({
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
        val decomposition = new Decomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val result = decomposition.mergeIfOverlap(decomposition.initializeSubprograms())
        assert(StringCompare.ignoreWhitespaces(result.programs, testCase.expectedOutput, testCase.className))
    })
  }

  "Enlarging subprograms" should "be correct" in {
    DecompositionUnitTest.enlargeSubprogramsUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val initialSubprograms: Set[Subprogram] = decomposition.initializeSubprograms()
        val result: Set[Subprogram] = initialSubprograms.map({
          subprogram =>
            val enlargeResult = decomposition.enlarge(subprogram)
            logger.debug(enlargeResult)
            enlargeResult
        })
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Computing environment modified set" should "be correct" in {
    DecompositionUnitTest.environmentModifiedSetUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val subprograms = decomposition.mergeIfOverlap(decomposition.initializeSubprograms())
        val result = decomposition.environmentModifiedSet(subprograms.programs.head, subprograms)
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Deciding subsequent execution" should "be correct" in {
    DecompositionUnitTest.subsequentExecutionUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val initialSubprograms: List[Subprogram] = decomposition.initializeSubprograms().toList
        val result = MathUtils.crossJoin2(initialSubprograms, initialSubprograms).map({
          case (subprogram1, subprogram2) =>
            logger.debug(s"Subprogram 1: $subprogram1")
            logger.debug(s"Subprogram 2: $subprogram2")
            decomposition.subsequentExecute(subprogram1, subprogram2)
        })
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Decomposition (Selective amortization)" should "succeed" in {
    DecompositionUnitTest.decomposeSelectiveAmortizationUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val result = decomposition.selectiveAmortize.groups.elements
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Inserting resets and counters" should "be correct" in {
    DecompositionUnitTest.insertingResetsAndUpdatesUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val subprograms = decomposition.mergeIfOverlap(decomposition.initializeSubprograms())
        val result = decomposition.insertGhostVariables(IntermediateResult(Groups(subprograms.programs), UNKNOWN))
        assert(StringCompare.ignoreWhitespaces(result.newSourceFileContents, testCase.expectedOutput, testCase.className))
    })
  }

  "Decomposition (No amortization)" should "succeed" in {
    DecompositionUnitTest.decomposeNoAmortizationUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val result = decomposition.noAmortize.groups.elements
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }

  "Decomposition (Full amortization)" should "succeed" in {
    DecompositionUnitTest.decomposeFullAmortizationUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val decomposition = new Decomposition(targetMethod, DEFAULT_ARGUMENTS, testMode = true)
        val result = decomposition.fullAmortize.groups.elements
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, testCase.className))
    })
  }
}

object DecompositionUnitTest {
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
      |        // if (start == text) break;
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
      """Subprogram(
        |R = R + 1;
        |)""".stripMargin

    val test02ExpectedOutput =
      """Subprogram(
        |R = R + separator;
        |)
        |Subprogram(
        |int index = 0
        |int start = 0
        |int end = 0
        |while (n > 0) {
        |    start = end + text;
        |    end = start + text;
        |    sb += start - index;
        |    R = R + (start - index);
        |    index = end;
        |}
        |sb += text - index;
        |R = R + (text - index);
        |)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", decompositionTest01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", decompositionTest02, test02ExpectedOutput),
    )
  }

  val decomposeNoAmortizationUnitTest: List[TestCaseJavaProgram] = {
    val test01ExpectedOutput =
      """Subprogram(
        |R = R + 1;
        |)""".stripMargin

    val test02ExpectedOutput =
      """Subprogram(
        |R = R + (start - index);
        |)
        |Subprogram(
        |R = R + (text - index);
        |)
        |Subprogram(
        |R = R + separator;
        |)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", decompositionTest01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", decompositionTest02, test02ExpectedOutput),
    )
  }

  val decomposeFullAmortizationUnitTest: List[TestCaseJavaProgram] = {
    val test01ExpectedOutput =
      """Subprogram(
        |int R = 0
        |int i = 0
        |while (i < n) {
        |    int j = 0;
        |    while (j < m) {
        |        j++;
        |        R = R + 1;
        |    }
        |    i++;
        |}
        |)""".stripMargin

    val test02ExpectedOutput =
      """Subprogram(
        |int R = 0
        |int sb = 0
        |int i = 0
        |while (i < templateds) {
        |    int index = 0;
        |    int start = 0;
        |    int end = 0;
        |    while (n > 0) {
        |        start = end + text;
        |        end = start + text;
        |        sb += start - index;
        |        R = R + (start - index);
        |        index = end;
        |    }
        |    sb += text - index;
        |    R = R + (text - index);
        |    sb += separator;
        |    R = R + separator;
        |    i++;
        |}
        |)""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", decompositionTest01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", decompositionTest02, test02ExpectedOutput),
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
        |R = R + 1;
        |)
        |Subprogram(
        |R = R + 2;
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
        |R = R + 1;
        |)
        |Subprogram(
        |R = R + b;
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
        |R = R + 1;
        |)
        |Subprogram(
        |R = R + b;
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
        |    if (d < n) {
        |      R = R + d;
        |    }
        |  }
        |}""".stripMargin
    val test04ExpectedOutput =
      """Subprogram(
        |{
        |    R = R + d;
        |}
        |)""".stripMargin

    val test05 =
      """class Test05 {
        |  void f(int n, int m)
        |  {
        |    int e = 0;
        |    int R = 0;
        |    if (e < n) {
        |      R = R + e;
        |    }
        |    else {
        |      e = e + 2;
        |    }
        |  }
        |}""".stripMargin
    val test05ExpectedOutput =
      """Subprogram(
        |{
        |    R = R + e;
        |}
        |)""".stripMargin

    val test06 =
      """class Test06 {
        |  void f(int n, int m)
        |  {
        |    int f = 0;
        |    int R = 0;
        |    for (int i = 0; i < n; i++) {
        |      R = R + f;
        |    }
        |  }
        |}""".stripMargin
    val test06ExpectedOutput =
      """Subprogram(
        |{
        |    R = R + f;
        |}
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
    val test02ExpectedOutput = "it"

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
        |  void f(int n, int m)
        |  {
        |    int C6 = 0;
        |    int C7 = 0;
        |    int D0 = 0;
        |    int D0p = 0;
        |    int D1 = 0;
        |    int D1p = 0;
        |    int it = n;
        |    int R = 0;
        |    while (it > 0)
        |    {
        |      it--;
        |      D0p = D0; D0 = 0; C6 = C6 + 1;D0 = D0 + m;
        |      R = R + m;
        |    }
        |    D1p = D1; D1 = 0; C7 = C7 + 1;D1 = D1 + n;
        |    R = R + n;
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
        |  void f(int n, int m)
        |  {
        |    int C7 = 0;
        |    int C8 = 0;
        |    int D0 = 0;
        |    int D0p = 0;
        |    int D1 = 0;
        |    int D1p = 0;
        |    int it = n;
        |    int R = 0;
        |    it++;
        |    while (it > 0)
        |    {
        |      it--;
        |      D0p = D0; D0 = 0; C7 = C7 + 1;D0 = D0 + 1;
        |      R = R + 1;
        |      D1p = D1; D1 = 0; C8 = C8 + 1;D1 = D1 + n;
        |      R = R + n;
        |    }
        |  }
        |}""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
    )
  }
}
