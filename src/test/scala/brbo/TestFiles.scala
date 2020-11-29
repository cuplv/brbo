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
}
