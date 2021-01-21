package brbo.common

import brbo.common.BeforeOrAfterOrThis.BEFORE
import brbo.common.InstrumentUtils.InstrumentMode.{ALL, AT_MOST_ONCE}
import brbo.common.InstrumentUtils.StatementTreeInstrumentation
import brbo.verification.{BasicProcessor, BoundChecking}
import brbo.{StringCompare, TestCaseJavaProgram}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashSet

class InstrumentUtilsUnitTest extends AnyFlatSpec {
  "Instrumentation" should "output correct java source code without any instrumentation" in {
    InstrumentUtilsUnitTest.noInstrumentUnitTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val result = InstrumentUtils.instrumentStatementTrees(
          targetMethod,
          StatementTreeInstrumentation(
            Locations(_ => false, BEFORE),
            _ => ""
          ),
          indent = 0
        )
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, s"Test ${testCase.className} failed!"))
    })
  }

  it should s"output correct java source code when replacing `R = R + e` with `d = d + e` in mode `$AT_MOST_ONCE`" in {
    InstrumentUtilsUnitTest.replaceResourceAssignmentsAtMostOnce.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val result = InstrumentUtils.substituteAtomicStatements(targetMethod, InstrumentUtils.defaultResourceAssignment, 0, AT_MOST_ONCE)
        assert(StringCompare.ignoreWhitespaces(result.result, testCase.expectedOutput, s"Test ${testCase.className} failed!"))
    })
  }

  it should s"output correct java source code when replacing `R = R + e` with `d = d + e` in mode `$ALL`" in {
    InstrumentUtilsUnitTest.replaceResourceAssignmentsAll.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val result = InstrumentUtils.substituteAtomicStatements(targetMethod, InstrumentUtils.defaultResourceAssignment, 0, ALL)
        assert(StringCompare.ignoreWhitespaces(result.result, testCase.expectedOutput, s"Test ${testCase.className} failed!"))
    })
  }

  "Treating counters as resource variables instrumentation" should "be correct" in {
    InstrumentUtilsUnitTest.treatCountersAsResourcesUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val result = InstrumentUtils.instrumentStatementTrees(
          targetMethod,
          BoundChecking.treatCounterAsResourceInstrumentation("C1", "R3"),
          indent = 0
        )
        assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, s"Test ${testCase.className} failed!"))
    })
  }
}

object InstrumentUtilsUnitTest {
  val noInstrumentUnitTests: HashSet[TestCaseJavaProgram] = {
    val assertTest =
      """class AssertTest {
        |    void f(int n) {
        |        assert(1 == 1);
        |    }
        |}""".stripMargin
    val assertTestExpected =
      """{
        |  assert (1 == 1);
        |}""".stripMargin

    val breakTest =
      """class BreakTest {
        |    void f(int n) {
        |        while (true) {
        |            break;
        |        }
        |    }
        |}""".stripMargin
    val breakTestExpected =
      """{
        |  while (true)
        |  {
        |    break;
        |  }
        |}""".stripMargin

    val continueTest =
      """class ContinueTest {
        |    void f(int n) {
        |        while (true) {
        |            continue;
        |        }
        |    }
        |}""".stripMargin
    val continueTestExpected =
      """{
        |  while (true)
        |  {
        |    continue;
        |  }
        |}""".stripMargin

    /*val doWhileTest =
      """class DoWhileTest {
        |    void f(int n) {
        |        int i = 0;
        |        do {
        |            i++;
        |        } while (i >= 0);
        |    }
        |}""".stripMargin
    val doWhileTestExpected =
      """{
        |  int i = 0;
        |  do
        |  {
        |    i++;;
        |  }
        |  while (i >= 0);
        |}""".stripMargin*/

    val emptyTest =
      """class EmptyTest {
        |    void f(int n) {
        |        ;
        |    }
        |}""".stripMargin
    val emptyTestExpected =
      """{
        |  ;
        |}""".stripMargin

    val forLoopTest =
      """class ForLoopTest {
        |    void f(int n) {
        |        for (int i = 0; i < 10; i++) {
        |            i--;
        |        }
        |    }
        |}""".stripMargin
    val forLoopTestExpected =
      """{
        |  {// For loop
        |    int i = 0;
        |    while (i < 10) {
        |      i--;
        |      i++;
        |    }
        |  }
        |}""".stripMargin

    val ifTest =
      """class IfTest {
        |    void f(int n) {
        |        if (n > 10) {
        |            int a = n + 1;
        |        }
        |        else {
        |            int b = n;
        |        }
        |    }
        |}""".stripMargin
    val ifTestExpected =
      """{
        |  if (n > 10)
        |  {
        |    int a = n + 1;
        |  }
        |  else
        |  {
        |    int b = n;
        |  }
        |}""".stripMargin

    val labelTest =
      """class LabelTest {
        |    void f(int n) {
        |        int i = 0;
        |        Label:
        |        i = 1;
        |    }
        |}""".stripMargin
    val labelTestExpected =
      """{
        |  int i = 0;
        |  Label:
        |  i = 1;
        |}""".stripMargin

    val returnTest =
      """class ReturnTest {
        |    int f(int n) {
        |        return n;
        |    }
        |}""".stripMargin
    val returnTestExpected =
      """{
        |  return n;
        |}""".stripMargin

    val variableTest =
      """class VariableTest {
        |    void f(int n) {
        |        int i = n + 1;
        |        int j = 0;
        |    }
        |}""".stripMargin
    val variableTestExpected =
      """{
        |  int i = n + 1;
        |  int j = 0;
        |}""".stripMargin

    val whileTest =
      """class WhileTest {
        |    void f(int n) {
        |        int i = 0;
        |        while (i < n) {
        |            i--;
        |        }
        |    }
        |}""".stripMargin
    val whileTestExpected =
      """{
        |  int i = 0;
        |  while (i < n)
        |  {
        |    i--;
        |  }
        |}""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("AssertTest", assertTest, assertTestExpected),
      TestCaseJavaProgram("BreakTest", breakTest, breakTestExpected),
      TestCaseJavaProgram("ContinueTest", continueTest, continueTestExpected),
      // TestCaseJavaProgram("DoWhileTest", doWhileTest, doWhileTestExpected),
      TestCaseJavaProgram("EmptyTest", emptyTest, emptyTestExpected),
      TestCaseJavaProgram("ForLoopTest", forLoopTest, forLoopTestExpected),
      TestCaseJavaProgram("IfTest", ifTest, ifTestExpected),
      TestCaseJavaProgram("LabelTest", labelTest, labelTestExpected),
      TestCaseJavaProgram("ReturnTest", returnTest, returnTestExpected),
      TestCaseJavaProgram("VariableTest", variableTest, variableTestExpected),
      TestCaseJavaProgram("WhileTest", whileTest, whileTestExpected)
    )
  }

  val replaceResourceAssignmentsAtMostOnce: HashSet[TestCaseJavaProgram] = {
    val noLoopTest1 =
      """class NoLoopTest1 {
        |    void f(int n) {
        |        int R = 0;
        |        R = R + n;
        |        R = 3;
        |    }
        |}""".stripMargin
    val noLoopTest1Expected =
      """{
        |  int R = 0;
        |  D100 = D100 + n;
        |  R = 3;
        |}""".stripMargin

    val noLoopTest2 =
      """class NoLoopTest2 {
        |    void f(int n) {
        |        int R = 0;
        |        R++;
        |    }
        |}""".stripMargin
    val noLoopTest2Expected =
      """{
        |  int R = 0;
        |  D100 = D100 + 1;
        |}""".stripMargin

    val whileLoopTest1 =
      """class WhileLoopTest1 {
        |    void f(int n) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            i++;
        |            R++;
        |        }
        |    }
        |}""".stripMargin
    val whileLoopTest1Expected =
      """{
        |  int R = 0;
        |  int i = 0;
        |  while (i < n)
        |  {
        |    i++;;
        |    D100 = D100 + 1;
        |  }
        |}""".stripMargin

    val whileLoopTest2 =
      """class WhileLoopTest2 {
        |    void f(int n) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            i++;
        |            R++;
        |            R = R + 2;
        |        }
        |    }
        |}""".stripMargin
    val whileLoopTest2Expected =
      """{
        |  int R = 0;
        |  int i = 0;
        |  while (i < n)
        |  {
        |    i++;;
        |    D100 = D100 + 1;
        |    R = R + 2;
        |  }
        |}""".stripMargin

    val forLoopTest1 =
      """class ForLoopTest1 {
        |    void f(int n) {
        |        int R = 0;
        |        for (int i = 0; i < n; i++) {
        |            R++;
        |        }
        |    }
        |}""".stripMargin
    val forLoopTest1Expected =
      """{
        |  int R = 0;
        |  {// For loop
        |    int i = 0;
        |    while (i < n) {
        |      D100 = D100 + 1;
        |      i++;
        |    }
        |  }
        |}""".stripMargin

    val forLoopTest2 =
      """class ForLoopTest2 {
        |    void f(int n) {
        |        int R = 0;
        |        for (int i = 0; i < n; i++) {
        |            R++;
        |        }
        |        R = R + 7;
        |    }
        |}""".stripMargin
    val forLoopTest2Expected =
      """{
        |  int R = 0;
        |  {// For loop
        |    int i = 0;
        |    while (i < n) {
        |      D100 = D100 + 1;
        |      i++;
        |    }
        |  }
        |  R = R + 7;
        |}""".stripMargin

    val sequenceTest1 =
      """class SequenceTest1 {
        |    void f(int n) {
        |        int R = 0;
        |        for (int i = 0; i < n; i++) {
        |            R++;
        |        }
        |        for (int j = 0; j < n; j++) {
        |            R = R + 3;
        |        }
        |    }
        |}""".stripMargin
    val sequenceTest1Expected =
      """{
        |  int R = 0;
        |  {// For loop
        |    int i = 0;
        |    while (i < n) {
        |      D100 = D100 + 1;
        |      i++;
        |    }
        |  }
        |  {// For loop
        |    int j = 0;
        |    while (j < n) {
        |      R = R + 3;
        |      j++;
        |    }
        |  }
        |}""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("NoLoopTest1", noLoopTest1, noLoopTest1Expected),
      TestCaseJavaProgram("NoLoopTest2", noLoopTest2, noLoopTest2Expected),
      TestCaseJavaProgram("WhileLoopTest1", whileLoopTest1, whileLoopTest1Expected),
      TestCaseJavaProgram("WhileLoopTest2", whileLoopTest2, whileLoopTest2Expected),
      TestCaseJavaProgram("ForLoopTest1", forLoopTest1, forLoopTest1Expected),
      TestCaseJavaProgram("ForLoopTest2", forLoopTest2, forLoopTest2Expected),
      TestCaseJavaProgram("SequenceTest1", sequenceTest1, sequenceTest1Expected),
    )
  }

  val replaceResourceAssignmentsAll: HashSet[TestCaseJavaProgram] = {
    val noLoopTest1 =
      """class NoLoopTest1 {
        |    void f(int n) {
        |        int R = 0;
        |        R = R + n;
        |        R = 3;
        |    }
        |}""".stripMargin
    // TODO: Not instrumenting resets to resource variables
    val noLoopTest1Expected =
      """{
        |  int R = 0;
        |  D100 = D100 + n;
        |  R = 3;;
        |}""".stripMargin

    val noLoopTest2 =
      """class NoLoopTest2 {
        |    void f(int n) {
        |        int R = 0;
        |        R++;
        |    }
        |}""".stripMargin
    val noLoopTest2Expected =
      """{
        |  int R = 0;
        |  D100 = D100 + 1;
        |}""".stripMargin

    val whileLoopTest1 =
      """class WhileLoopTest1 {
        |    void f(int n) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            i++;
        |            R++;
        |        }
        |    }
        |}""".stripMargin
    val whileLoopTest1Expected =
      """{
        |  int R = 0;
        |  int i = 0;
        |  while (i < n)
        |  {
        |    i++;;
        |    D100 = D100 + 1;
        |  }
        |}""".stripMargin

    val whileLoopTest2 =
      """class WhileLoopTest2 {
        |    void f(int n) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            i++;
        |            R++;
        |            R = R + 2;
        |        }
        |    }
        |}""".stripMargin
    val whileLoopTest2Expected =
      """{
        |  int R = 0;
        |  int i = 0;
        |  while (i < n)
        |  {
        |    i++;;
        |    D100 = D100 + 1;
        |    D100 = D100 + 2;
        |  }
        |}""".stripMargin

    val forLoopTest1 =
      """class ForLoopTest1 {
        |    void f(int n) {
        |        int R = 0;
        |        for (int i = 0; i < n; i++) {
        |            R++;
        |        }
        |    }
        |}""".stripMargin
    val forLoopTest1Expected =
      """{
        |  int R = 0;
        |  {// For loop
        |    int i = 0;
        |    while (i < n) {
        |      D100 = D100 + 1;
        |      i++;;
        |    }
        |  }
        |}""".stripMargin

    val forLoopTest2 =
      """class ForLoopTest2 {
        |    void f(int n) {
        |        int R = 0;
        |        for (int i = 0; i < n; i++) {
        |            R++;
        |        }
        |        R = R + 7;
        |    }
        |}""".stripMargin
    val forLoopTest2Expected =
      """{
        |  int R = 0;
        |  {// For loop
        |    int i = 0;
        |    while (i < n) {
        |      D100 = D100 + 1;
        |      i++;;
        |    }
        |  }
        |  D100 = D100 + 7;
        |}""".stripMargin

    val sequenceTest1 =
      """class SequenceTest1 {
        |    void f(int n) {
        |        int R = 0;
        |        for (int i = 0; i < n; i++) {
        |            R++;
        |        }
        |        for (int j = 0; j < n; j++) {
        |            R = R + 3;
        |        }
        |    }
        |}""".stripMargin
    val sequenceTest1Expected =
      """{
        |  int R = 0;
        |  {// For loop
        |    int i = 0;
        |    while (i < n) {
        |      D100 = D100 + 1;
        |      i++;;
        |    }
        |  }
        |  {// For loop
        |    int j = 0;
        |    while (j < n) {
        |      D100 = D100 + 3;
        |      j++;;
        |    }
        |  }
        |}""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("NoLoopTest1", noLoopTest1, noLoopTest1Expected),
      TestCaseJavaProgram("NoLoopTest2", noLoopTest2, noLoopTest2Expected),
      TestCaseJavaProgram("WhileLoopTest1", whileLoopTest1, whileLoopTest1Expected),
      TestCaseJavaProgram("WhileLoopTest2", whileLoopTest2, whileLoopTest2Expected),
      TestCaseJavaProgram("ForLoopTest1", forLoopTest1, forLoopTest1Expected),
      TestCaseJavaProgram("ForLoopTest2", forLoopTest2, forLoopTest2Expected),
      TestCaseJavaProgram("SequenceTest1", sequenceTest1, sequenceTest1Expected),
    )
  }

  val treatCountersAsResourcesUnitTest: List[TestCaseJavaProgram] = {
    val test01: String =
      """class Test01 {
        |  void f(int n)
        |  {
        |    int D100 = 0;
        |    int C1 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      C1 = 0;
        |      i++;
        |      C1 = C1 + 1;
        |      D100 = 0;
        |      D100 = D100 + 1;
        |      R = R + 1;
        |    }
        |  }
        |}""".stripMargin
    val test01ExpectedOutput =
      """{
        |
        |  int R3 = 0;
        |
        |  int i = 0;
        |  while (i < n)
        |  {
        |    R3 = 0
        |    i++;
        |    R3 = R3 + 1;
        |
        |
        |
        |  }
        |}""".stripMargin

    val test02: String =
      """class Test02 {
        |  void f(int n, int m, int l)
        |  {
        |    int R = 0;
        |    int C1 = 0;
        |    int D100 = 0;
        |    int i = 0;
        |    C1 = 0;
        |    while (i < n) {
        |      int j = 0;
        |      C1 = C1 + 1;
        |      D100 = 0;
        |      while (j < m) {
        |        j++;
        |        R = R + 1;
        |        D100 = D100 + 1;
        |      }
        |      i++;
        |    }
        |  }
        |}""".stripMargin
    val test02ExpectedOutput =
      """{
        |
        |  int R3 = 0;
        |
        |  int i = 0;
        |  R3 = 0
        |  while (i < n)
        |  {
        |    int j = 0;
        |    R3 = R3 + 1;
        |
        |    while (j < m)
        |    {
        |      j++;
        |
        |
        |    }
        |    i++;
        |  }
        |}""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", test01, test01ExpectedOutput),
      TestCaseJavaProgram("Test02", test02, test02ExpectedOutput),
    )
  }
}
