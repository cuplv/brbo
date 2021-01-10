package brbo.common

import brbo.TestCaseJavaProgram
import brbo.boundinference.BasicProcessor
import brbo.common.InstrumentUtils.AtomicStatementInstrumentation
import brbo.common.InstrumentUtils.InstrumentMode.{ALL, AT_MOST_ONCE}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashSet

class InstrumentUtilsUnitTest extends AnyFlatSpec {
  "Instrumentation" should "output correct java source code without any instrumentation" in {
    InstrumentUtilsUnitTest.noInstrumentUnitTests.foreach({
      testCase =>
        val basicProcessor = new BasicProcessor
        JavacUtils.runProcessor(testCase.className, testCase.inputProgram, basicProcessor)
        val results = basicProcessor.testInstrumentation(AtomicStatementInstrumentation(_ => false, tree => tree.toString), AT_MOST_ONCE)
        assert(results.size == 1, "We should have only 1 method per test class")
        results.foreach({
          case (_, result) => assert(result.result == testCase.expectedOutput, s"Test ${testCase.className} failed!")
        })
    })
  }

  it should s"output correct java source code when replacing `R = R + e` with `d = d + e` in mode `$AT_MOST_ONCE`" in {
    InstrumentUtilsUnitTest.replaceResourceAssignmentsAtMostOnce.foreach({
      testCase =>
        val basicProcessor = new BasicProcessor
        JavacUtils.runProcessor(testCase.className, testCase.inputProgram, basicProcessor)
        val results = basicProcessor.testInstrumentation(InstrumentUtils.defaultResourceAssignment, AT_MOST_ONCE)

        assert(results.size == 1, "We should have exactly 1 method per test class")
        results.foreach({
          case (_, result) =>
            // println(result.result)
            // println(testCase.expectedOutput)
            assert(result.result == testCase.expectedOutput, s"Test ${testCase.className} failed!")
        })
    })
  }

  it should s"output correct java source code when replacing `R = R + e` with `d = d + e` in mode `$ALL`" in {
    InstrumentUtilsUnitTest.replaceResourceAssignmentsAll.foreach({
      testCase =>
        val basicProcessor = new BasicProcessor
        JavacUtils.runProcessor(testCase.className, testCase.inputProgram, basicProcessor)
        val results = basicProcessor.testInstrumentation(InstrumentUtils.defaultResourceAssignment, ALL)

        assert(results.size == 1, "We should have exactly 1 method per test class")
        results.foreach({
          case (_, result) =>
            // println(result.result)
            // println(testCase.expectedOutput)
            assert(result.result == testCase.expectedOutput, s"Test ${testCase.className} failed!")
        })
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
        |  assert (1 == 1);;
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
        |    break;;
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
        |    continue;;
        |  }
        |}""".stripMargin

    val doWhileTest =
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
        |}""".stripMargin

    val emptyTest =
      """class EmptyTest {
        |    void f(int n) {
        |        ;
        |    }
        |}""".stripMargin
    val emptyTestExpected =
      """{
        |  ;;
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
        |      i--;;
        |      i++;;
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
        |        int i;
        |        Label:
        |        i = 1;
        |    }
        |}""".stripMargin
    val labelTestExpected =
      """{
        |  int i;
        |  Label:
        |  i = 1;;
        |}""".stripMargin

    val returnTest =
      """class ReturnTest {
        |    int f(int n) {
        |        return n;
        |    }
        |}""".stripMargin
    val returnTestExpected =
      """{
        |  return n;;
        |}""".stripMargin

    val variableTest =
      """class VariableTest {
        |    void f(int n) {
        |        int i = n + 1;
        |        int j;
        |    }
        |}""".stripMargin
    val variableTestExpected =
      """{
        |  int i = n + 1;
        |  int j;
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
        |    i--;;
        |  }
        |}""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("AssertTest", assertTest, assertTestExpected),
      TestCaseJavaProgram("BreakTest", breakTest, breakTestExpected),
      TestCaseJavaProgram("ContinueTest", continueTest, continueTestExpected),
      TestCaseJavaProgram("DoWhileTest", doWhileTest, doWhileTestExpected),
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
        |        for (int i = 0; i < n; i++) {
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
        |    int i = 0;
        |    while (i < n) {
        |      R = R + 3;
        |      i++;
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
        |        for (int i = 0; i < n; i++) {
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
        |    int i = 0;
        |    while (i < n) {
        |      D100 = D100 + 3;
        |      i++;;
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
}