package brbo.common.instrument

import brbo.{StringCompare, TestCaseJavaProgram}
import brbo.verification.BasicProcessor
import org.scalatest.flatspec.AnyFlatSpec

class ChangeEntryNodeUnitTest extends AnyFlatSpec {
  "Changing entry nodes of programs" should "succeed" in {
    ChangeEntryNodeUnitTest.changeEntryNodeUnitTest.foreach {
      testCase =>
        val inputMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val entryCommand = inputMethod.commands.filter(t => t.toString == "int y = 2").head
        val newMethod = ChangeEntryNode.changeEntryNode(inputMethod, entryCommand, Set())
        assert(StringCompare.ignoreWhitespaces(newMethod.methodTree.toString, testCase.expectedOutput, testCase.className))
    }
  }
}

object ChangeEntryNodeUnitTest {
  val changeEntryNodeUnitTest: List[TestCaseJavaProgram] = {
    val block: String =
      """class Block {
        |  void f(int n)
        |  {
        |    int x = 1;
        |    int y = 2;
        |    int z = 3;
        |  }
        |}""".stripMargin
    val blockExpected =
      """void f(int n, int x, int y, int z) {
        |    {
        |        z = 3;
        |        if (true) return;
        |    }
        |}""".stripMargin

    val whileLoop: String = // A loop with a nesting depth of 1
      """class WhileLoop {
        |  void f(int n)
        |  {
        |    while (1 < n) {
        |      int x = 1;
        |      int y = 2;
        |      int z = 3;
        |    }
        |  }
        |}""".stripMargin
    val whileLoopExpected =
      """void f(int n, int x, int y, int z) {
        |    {
        |        z = 3;
        |        {
        |            while (1 < n) {
        |                x = 1;
        |                if (true) return;
        |                z = 3;
        |            }
        |            {
        |                if (true) return;
        |            }
        |        }
        |    }
        |}""".stripMargin

    val forLoop: String = // A loop with a nesting depth of 2
      """class ForLoop {
        |  void f(int n)
        |  {
        |    for (int i = 0; i < 3; i++) {
        |      int x = 1;
        |      int y = 2;
        |      int z = 3;
        |    }
        |  }
        |}""".stripMargin
    val forLoopExpected =
      """void f(int i, int n, int x, int y, int z) {
        |    {
        |        z = 3;
        |        {
        |            while (i < 3) {
        |                {
        |                    x = 1;
        |                    if (true) return;
        |                    z = 3;
        |                }
        |                i++;
        |            }
        |            {
        |                if (true) return;
        |            }
        |        }
        |    }
        |}""".stripMargin

    val nestedLoop01: String =
      """class NestedLoop01 {
        |  void f(int n)
        |  {
        |    while (1 < n) {
        |      while (3 < n) {
        |        int x = 1;
        |        int y = 2;
        |        int z = 3;
        |      }
        |    }
        |  }
        |}""".stripMargin
    val nestedLoop01Expected =
      """void f(int n, int x, int y, int z) {
        |    {
        |        z = 3;
        |        {
        |            while (3 < n) {
        |                x = 1;
        |                if (true) return;
        |                z = 3;
        |            }
        |            {
        |                {
        |                    while (1 < n) {
        |                        while (3 < n) {
        |                            x = 1;
        |                            if (true) return;
        |                            z = 3;
        |                        }
        |                    }
        |                    {
        |                        if (true) return;
        |                    }
        |                }
        |            }
        |        }
        |    }
        |}""".stripMargin

    val nestedLoop02: String =
      """class NestedLoop02 {
        |  void f(int n)
        |  {
        |    while (1 < n) {
        |      while (3 < n) {
        |      }
        |      int x = 1;
        |      int y = 2;
        |      int z = 3;
        |    }
        |  }
        |}""".stripMargin
    val nestedLoop02Expected =
      """void f(int n, int x, int y, int z) {
        |    {
        |        z = 3;
        |        {
        |            while (1 < n) {
        |                while (3 < n) {
        |                }
        |                x = 1;
        |                if (true) return;
        |                z = 3;
        |            }
        |            {
        |                if (true) return;
        |            }
        |        }
        |    }
        |}
        |""".stripMargin

    val nestedLoop03: String =
      """class NestedLoop03 {
        |  void f(int n)
        |  {
        |    while (1 < n) {
        |      int x = 1;
        |      int y = 2;
        |      int z = 3;
        |      while (3 < n) {
        |      }
        |    }
        |  }
        |}""".stripMargin
    val nestedLoop03Expected =
      """void f(int n, int x, int y, int z) {
        |    {
        |        z = 3;
        |        while (3 < n) {
        |        }
        |        {
        |            while (1 < n) {
        |                x = 1;
        |                if (true) return;
        |                z = 3;
        |                while (3 < n) {
        |                }
        |            }
        |            {
        |                if (true) return;
        |            }
        |        }
        |    }
        |}
        |""".stripMargin

    val ifLoop01: String =
      """class IfLoop01 {
        |  void f(int n)
        |  {
        |    while (1 < n) {
        |      if (3 < n) {
        |        int x = 1;
        |        int y = 2;
        |        int z = 3;
        |      }
        |      else {
        |        int w = 4;
        |      }
        |    }
        |  }
        |}""".stripMargin
    val ifLoop01Expected =
      """void f(int n, int w, int x, int y, int z) {
        |    {
        |        z = 3;
        |        {
        |            {
        |                while (1 < n) {
        |                    if (3 < n) {
        |                        x = 1;
        |                        if (true) return;
        |                        z = 3;
        |                    } else {
        |                        w = 4;
        |                    }
        |                }
        |                {
        |                    if (true) return;
        |                }
        |            }
        |        }
        |    }
        |}""".stripMargin

    val ifLoop02: String =
      """class IfLoop02 {
        |  void f(int n)
        |  {
        |    while (1 < n) {
        |      int a = 5;
        |      if (3 < 4) {
        |        int w = 4;
        |      }
        |      else {
        |        int x = 1;
        |        int y = 2;
        |        int z = 3;
        |      }
        |      int b = 6;
        |    }
        |  }
        |}""".stripMargin
    val ifLoop02Expected =
      """void f(int a, int b, int n, int w, int x, int y, int z) {
        |    {
        |        z = 3;
        |        {
        |            b = 6;
        |            {
        |                while (1 < n) {
        |                    a = 5;
        |                    if (3 < 4) {
        |                        w = 4;
        |                    } else {
        |                        x = 1;
        |                        if (true) return;
        |                        z = 3;
        |                    }
        |                    b = 6;
        |                }
        |                {
        |                    if (true) return;
        |                }
        |            }
        |        }
        |    }
        |}""".stripMargin

    List[TestCaseJavaProgram](
      TestCaseJavaProgram("Block", block, blockExpected),
      TestCaseJavaProgram("WhileLoop", whileLoop, whileLoopExpected),
      TestCaseJavaProgram("ForLoop", forLoop, forLoopExpected),
      TestCaseJavaProgram("NestedLoop01", nestedLoop01, nestedLoop01Expected),
      TestCaseJavaProgram("NestedLoop02", nestedLoop02, nestedLoop02Expected),
      TestCaseJavaProgram("NestedLoop03", nestedLoop03, nestedLoop03Expected),
      TestCaseJavaProgram("IfLoop01", ifLoop01, ifLoop01Expected),
      TestCaseJavaProgram("IfLoop02", ifLoop02, ifLoop02Expected),
    )
  }
}
