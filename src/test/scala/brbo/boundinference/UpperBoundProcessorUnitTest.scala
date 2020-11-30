package brbo.boundinference

import brbo.TestCase
import brbo.common.JavacUtils
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashSet

class UpperBoundProcessorUnitTest extends AnyFlatSpec {
  "UpperBoundProcessorUnitTest" should "correctly insert accumulation contexts" in {
    UpperBoundProcessorUnitTest.insertAccumulationContextTests.foreach({
      testCase =>
        val upperBoundProcessor = new UpperBoundProcessor("", "", Set.empty)
        JavacUtils.runProcessor(testCase.name, testCase.sourceCode, upperBoundProcessor)
        val result = {
          val results = upperBoundProcessor.placeAccumulationContexts()
          results.mkString("\n")
        }
        // println(result)
        // println(testCase.expectedOutput)
        assert(result == testCase.expectedOutput)
    })
  }
}

object UpperBoundProcessorUnitTest {
  val insertAccumulationContextTests: HashSet[TestCase] = {
    val test01 =
      """class Test01 {
        |  void f(int n)
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      i++;
        |      D100 = D100 + 1;
        |    }
        |  }
        |}""".stripMargin
    val test01Expected = ""

    val test02 =
      """class Test02 {
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
        |        j++;
        |        D100 = D100 + 1;
        |      }
        |      i++;
        |    }
        |  }
        |}""".stripMargin
    val test02Expected =
      """  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while ((i < n))
        |    {
        |      int j;
        |      j = 0;;
        |      ;;
        |      while ((j < m))
        |      {
        |        j++;;
        |        D100 = D100 + 1;;
        |      }
        |      i++;; D100 = 0;;
        |    }
        |  }
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while ((i < n))
        |    {
        |      int j; D100 = 0;;
        |      j = 0;
        |      ;;
        |      while ((j < m))
        |      {
        |        j++;
        |        D100 = D100 + 1;
        |      }
        |      i++;
        |    }
        |  }""".stripMargin

    val test03 =
      """class Test03 {
        |  void f(int n, int m)
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      int j;
        |      j = 0;
        |      while (j < m)
        |      {
        |        j = j + 4;
        |        D100 = D100 + 1;
        |      }
        |      j = 3;
        |      while (j < m)
        |      {
        |        j++;
        |        D100 = D100 + 2;
        |      }
        |      i++;
        |    }
        |  }
        |}""".stripMargin
    val test03Expected =
      """  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while ((i < n))
        |    {
        |      int j;
        |      j = 0;;
        |      while ((j < m))
        |      {
        |        j = j + 4;;
        |        D100 = D100 + 1;;
        |      }
        |      j = 3;;
        |      while ((j < m))
        |      {
        |        j++;;
        |        D100 = D100 + 2;;
        |      }
        |      i++;; D100 = 0;;
        |    }
        |  }
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while ((i < n))
        |    {
        |      int j;
        |      j = 0;;
        |      while ((j < m))
        |      {
        |        j = j + 4;;
        |        D100 = D100 + 1;;
        |      }
        |      j = 3;;
        |      while ((j < m))
        |      {
        |        j++;; D100 = 0;;
        |        D100 = D100 + 2;
        |      }
        |      i++;
        |    }
        |  }
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while ((i < n))
        |    {
        |      int j;
        |      j = 0;;
        |      while ((j < m))
        |      {
        |        j = j + 4;;
        |        D100 = D100 + 1;;
        |      }
        |      j = 3;; D100 = 0;;
        |      while ((j < m))
        |      {
        |        j++;
        |        D100 = D100 + 2;
        |      }
        |      i++;
        |    }
        |  }
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while ((i < n))
        |    {
        |      int j;
        |      j = 0;;
        |      while ((j < m))
        |      {
        |        j = j + 4;; D100 = 0;;
        |        D100 = D100 + 1;
        |      }
        |      j = 3;
        |      while ((j < m))
        |      {
        |        j++;
        |        D100 = D100 + 2;
        |      }
        |      i++;
        |    }
        |  }
        |  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while ((i < n))
        |    {
        |      int j; D100 = 0;;
        |      j = 0;
        |      while ((j < m))
        |      {
        |        j = j + 4;
        |        D100 = D100 + 1;
        |      }
        |      j = 3;
        |      while ((j < m))
        |      {
        |        j++;
        |        D100 = D100 + 2;
        |      }
        |      i++;
        |    }
        |  }""".stripMargin

    val test04 =
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
    val test04Expected =
      """  {
        |    int D100 = 0;
        |    int R = 0;
        |    int i = 0;
        |    while ((i < n))
        |    {
        |      int j = 0; D100 = 0;;
        |      while ((j < m))
        |      {
        |        j++;
        |        ;;
        |        D100 = D100 + 1;
        |      }
        |    }
        |    D100 = D100 + 3;
        |  }""".stripMargin

    HashSet[TestCase](
      TestCase("Test01", test01, test01Expected),
      TestCase("Test02", test02, test02Expected),
      TestCase("Test03", test03, test03Expected),
      TestCase("Test04", test04, test04Expected)
    )
  }
}