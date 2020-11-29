package brbo

import scala.collection.immutable.{HashMap, HashSet}

object TestFiles {
  private val sourceCode1 =
    """class Test01 {
      |    void f(int n) {
      |        int R = 0;
      |        int i = 0;
      |        while (i < n) {
      |            i++;
      |            R++;
      |            R = R + 2;
      |            i++;
      |        }
      |        i = i + 1;
      |        i = i + 2;
      |    }
      |}""".stripMargin

  private val sourceCode2 =
    """class Test02 {
      |    void f(int n, int m) {
      |        int R = 0;
      |        int i = 0;
      |        Label: while (i < n) {
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

  private val sourceCode3 =
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

  private val sourceCode4 =
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
      |
      |        for (int j = 0; j < l; j++) {
      |            R++;
      |        }
      |
      |        int k = 0;
      |        do {
      |            i++;
      |            R++;
      |        } while (k < n);
      |    }
      |}""".stripMargin

  val integrationTests: HashMap[String, String] =
    HashMap[String, String](
      "Test01" -> sourceCode1,
      //"Test02" -> sourceCode2,
      //"Test03" -> sourceCode3,
      //"Test04" -> sourceCode4
    )

  val unitTests: HashSet[TestCase] = {
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
        |  while ((true))
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
        |  while ((true))
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
        |  while ((i >= 0));
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
        |  if ((n > 10)) {
        |    {
        |      int a = n + 1;
        |    }
        |  } else {
        |    {
        |      int b = n;
        |    }
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
        |  while ((i < n))
        |  {
        |    i--;;
        |  }
        |}""".stripMargin

    HashSet[TestCase](
      TestCase("AssertTest", assertTest, assertTestExpected),
      TestCase("BreakTest", breakTest, breakTestExpected),
      TestCase("ContinueTest", continueTest, continueTestExpected),
      TestCase("DoWhileTest", doWhileTest, doWhileTestExpected),
      TestCase("EmptyTest", emptyTest, emptyTestExpected),
      TestCase("ForLoopTest", forLoopTest, forLoopTestExpected),
      TestCase("IfTest", ifTest, ifTestExpected),
      TestCase("LabelTest", labelTest, labelTestExpected),
      TestCase("ReturnTest", returnTest, returnTestExpected),
      TestCase("VariableTest", variableTest, variableTestExpected),
      TestCase("WhileTest", whileTest, whileTestExpected)
    )
  }

  val replaceResourceAssignments: HashSet[TestCase] = {
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
        |  while ((i < n))
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
        |  while ((i < n))
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

    val sequenceTest1 =
      """class SequenceTest1 {
        |    void f(int n) {
        |        int R = 0;
        |        for (int i = 0; i < n; i++) {
        |            R++;
        |        }
        |        R = R + 5;
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
        |  R = R + 5;
        |}""".stripMargin

    HashSet[TestCase](
      TestCase("NoLoopTest1", noLoopTest1, noLoopTest1Expected),
      TestCase("NoLoopTest2", noLoopTest2, noLoopTest2Expected),
      TestCase("WhileLoopTest1", whileLoopTest1, whileLoopTest1Expected),
      TestCase("WhileLoopTest2", whileLoopTest2, whileLoopTest2Expected),
      TestCase("ForLoopTest1", forLoopTest1, forLoopTest1Expected),
      TestCase("SequenceTest1", sequenceTest1, sequenceTest1Expected),
    )
  }

  case class TestCase(name: String, sourceCode: String, expectedOutput: String)

}
