package brbo.common

import org.scalatest.flatspec.AnyFlatSpec

class CFGUtilsTest extends AnyFlatSpec {
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

  "CFG processor" should "produce CFGs" in {
    val cfg = JavacUtils.runCFGProcessor("Test01", "f", "Test01", sourceCode1)
    assert(cfg != null)
    println(cfg)
  }
}
