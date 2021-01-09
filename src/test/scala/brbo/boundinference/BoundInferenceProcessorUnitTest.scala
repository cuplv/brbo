package brbo.boundinference

import brbo.TestCase
import brbo.common.JavacUtils
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashSet

@deprecated
class BoundInferenceProcessorUnitTest extends AnyFlatSpec {
  "BoundInferenceProcessor" should "correctly replace resource updates with delta variable updates in the default instrumentation" in {
    BoundInferenceProcessorUnitTest.sourceCodeDeltaUpdatesTests.foreach({
      testCase =>
        val boundInferenceProcessor = new BoundInferenceProcessor
        JavacUtils.runProcessor(testCase.name, testCase.input, boundInferenceProcessor)
        val result = boundInferenceProcessor.generateSourceCodeDeltaUpdates()
        // println(result)
        // println(testCase.expectedOutput)
        assert(result == testCase.expectedOutput, s"Test ${testCase.name} failed!")
    })
  }

  "BoundInferenceProcessor" should "correctly remove resource updates" in {
    BoundInferenceProcessorUnitTest.sourceCodeNoResourceUpdatesTests.foreach({
      testCase =>
        val boundInferenceProcessor = new BoundInferenceProcessor
        JavacUtils.runProcessor(testCase.name, testCase.input, boundInferenceProcessor)
        val result = boundInferenceProcessor.generateSourceCodeNoResourceUpdates()
        // println(result)
        // println(testCase.expectedOutput)
        assert(result == testCase.expectedOutput, s"Test ${testCase.name} failed!")
    })
  }
}

object BoundInferenceProcessorUnitTest {
  val sourceCodeDeltaUpdatesTests: HashSet[TestCase] = {
    val test01 =
      """class Test01 {
        |    void f(int n) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            i++;
        |            R++;
        |        }
        |    }
        |}""".stripMargin
    val test01Expected =
      """class Test01 {
        |  void f(int n)
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      i++;;
        |      D100 = D100 + 1;
        |    }
        |  }
        |}""".stripMargin

    val test02 =
      """class Test02 {
        |    void f(int n, int m) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            i++;
        |            R++;
        |        }
        |
        |        i = 0;
        |        while (i < m) {
        |            i++;
        |            R++;
        |        }
        |    }
        |}""".stripMargin
    val test02Expected =
      """class Test02 {
        |  void f(int n, int m)
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      i++;;
        |      D100 = D100 + 1;
        |    }
        |    i = 0;;
        |    while (i < m)
        |    {
        |      i++;;
        |      D100 = D100 + 1;
        |    }
        |  }
        |}""".stripMargin

    val test03 =
      """class Test03 {
        |    void f(int n, int m) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            int j;
        |            j = 0;
        |            while (j < m) {
        |                j++;
        |                R++;
        |            }
        |            i++;
        |        }
        |    }
        |}""".stripMargin
    val test03Expected =
      """class Test03 {
        |  void f(int n, int m)
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      int j;
        |      j = 0;;
        |      while (j < m)
        |      {
        |        j++;;
        |        D100 = D100 + 1;
        |      }
        |      i++;;
        |    }
        |  }
        |}""".stripMargin

    val test04 =
      """class Test04 {
        |    void f(int n, int m, int l) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            int j = 0;
        |            while (j < m) {
        |                j++;
        |                R++;
        |            }
        |        }
        |        R = R + 3;
        |    }
        |}""".stripMargin
    val test04Expected =
      """class Test04 {
        |  void f(int n, int m, int l)
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      int j = 0;
        |      while (j < m)
        |      {
        |        j++;;
        |        D100 = D100 + 1;
        |      }
        |    }
        |    D100 = D100 + 3;
        |  }
        |}""".stripMargin

    HashSet[TestCase](
      TestCase("Test01", test01, test01Expected),
      TestCase("Test02", test02, test02Expected),
      TestCase("Test03", test03, test03Expected),
      TestCase("Test04", test04, test04Expected)
    )
  }

  val sourceCodeNoResourceUpdatesTests: HashSet[TestCase] = {
    val test01 =
      """class Test01 {
        |    void f(int n) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            i++;
        |            R++;
        |        }
        |    }
        |}""".stripMargin
    val test01Expected =
      """class Test01 {
        |  void f(int n)
        |  {
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      i++;;
        |      ;;
        |    }
        |  }
        |}""".stripMargin

    val test02 =
      """class Test02 {
        |    void f(int n, int m) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            i++;
        |            R++;
        |        }
        |
        |        i = 0;
        |        while (i < m) {
        |            i++;
        |            R++;
        |        }
        |    }
        |}""".stripMargin
    val test02Expected =
      """class Test02 {
        |  void f(int n, int m)
        |  {
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      i++;;
        |      ;;
        |    }
        |    i = 0;;
        |    while (i < m)
        |    {
        |      i++;;
        |      ;;
        |    }
        |  }
        |}""".stripMargin

    val test03 =
      """class Test03 {
        |    void f(int n, int m) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            int j;
        |            j = 0;
        |            while (j < m) {
        |                j++;
        |                R++;
        |            }
        |            i++;
        |        }
        |    }
        |}""".stripMargin
    val test03Expected =
      """class Test03 {
        |  void f(int n, int m)
        |  {
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      int j;
        |      j = 0;;
        |      while (j < m)
        |      {
        |        j++;;
        |        ;;
        |      }
        |      i++;;
        |    }
        |  }
        |}""".stripMargin

    val test04 =
      """class Test04 {
        |    void f(int n, int m, int l) {
        |        int R = 0;
        |        int i = 0;
        |        while (i < n) {
        |            int j = 0;
        |            while (j < m) {
        |                j++;
        |                R++;
        |            }
        |        }
        |        R = R + 3;
        |    }
        |}""".stripMargin
    val test04Expected =
      """class Test04 {
        |  void f(int n, int m, int l)
        |  {
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      int j = 0;
        |      while (j < m)
        |      {
        |        j++;;
        |        ;;
        |      }
        |    }
        |    ;;
        |  }
        |}""".stripMargin

    HashSet[TestCase](
      TestCase("Test01", test01, test01Expected),
      TestCase("Test02", test02, test02Expected),
      TestCase("Test03", test03, test03Expected),
      TestCase("Test04", test04, test04Expected)
    )
  }
}