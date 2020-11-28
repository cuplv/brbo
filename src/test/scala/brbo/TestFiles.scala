package brbo

import scala.collection.immutable.HashMap

object TestFiles {
  private val sourceCode1 =
    """class Test01 {
      |    void f(int n) {
      |        int D = 0;
      |        int i = 0;
      |        while (i < n) {
      |            i++;
      |            D++;
      |        }
      |    }
      |}""".stripMargin

  val testFiles = HashMap[String, String]("Test01" -> sourceCode1)
  // TODO: More test files
}
