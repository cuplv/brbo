package brbo

import brbo.boundinference.BoundInferenceProcessor
import brbo.common.JavacUtils

object BrboMain {
  def main(args: Array[String]) {
    println("Hello World!")

    val sourceCode1 =
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

    val boundInferenceProcessor = new BoundInferenceProcessor
    JavacUtils.runProcessor("Test01", sourceCode1, boundInferenceProcessor)
  }
}
