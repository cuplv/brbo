package brbo.verification.dependency.reachdef

import brbo.verification.{BasicProcessor, BoundCheckingUnitTest}
import brbo.{StringCompare, TestCaseJavaProgram}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.analysis.{AnalysisResult, ForwardAnalysisImpl}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

class ReachingDefinitionUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[ReachingDefinitionUnitTest])

  "Reaching definition analysis" should "be correct" in {
    val transfer = new ReachingTransfer()
    val forwardAnalysisImpl = new ForwardAnalysisImpl[ReachingValue, ReachingStore, ReachingTransfer](transfer)

    ReachingDefinitionUnitTest.reachingDefinitionUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val cfg = targetMethod.cfg
        forwardAnalysisImpl.performAnalysis(cfg)
        val result: AnalysisResult[ReachingValue, ReachingStore] = forwardAnalysisImpl.getResult
        val output = cfg.getAllNodes.asScala.map({
          node =>
            s"Node: `$node` -> Store: ${
              result.getStoreBefore(node).definitions.toList.sortWith({
                (value1, value2) => value1.toString < value2.toString
              })
            }"
        }).sorted.mkString("\n")
        assert(StringCompare.ignoreWhitespaces(output, testCase.expectedOutput))
      // TODO: I have no idea why this doesn't work
      /*TreeUtils.collectCommands(targetMethod.methodTree.getBody).foreach({
        statement =>
          println(cfg.getNodesCorrespondingToTree(statement))
          println(s"Command: `$statement` -> Store: ${result.getStoreBefore(statement)}")
      })*/
    })
  }
}

object ReachingDefinitionUnitTest {
  val reachingDefinitionUnitTest: HashSet[TestCaseJavaProgram] = {
    val test01ExpectedOutput =
      """Node: `(C1 + 1)` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `(D100 + 1)` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `(R + 1)` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = (D100 + 1)`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `(i + 1)` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `i` in `i = 0`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `(i < n)` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `i` in `i = 0`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `0` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `0` -> Store: List(`C1` in `C1 = 0`, `D100` in `D100 = 0`, `R` in `R = 0`, `i` in `i`, `n` (input))
        |Node: `0` -> Store: List(`C1` in `C1 = 0`, `D100` in `D100 = 0`, `R` in `R`, `n` (input))
        |Node: `0` -> Store: List(`C1` in `C1`, `D100` in `D100 = 0`, `n` (input))
        |Node: `0` -> Store: List(`D100` in `D100`, `n` (input))
        |Node: `1` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `i` in `i = 0`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `1` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `1` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = (D100 + 1)`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `1` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `C1 = (C1 + 1)` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `C1 = 0` -> Store: List(`C1` in `C1`, `D100` in `D100 = 0`, `n` (input))
        |Node: `C1` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `C1` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `C1` -> Store: List(`D100` in `D100 = 0`, `n` (input))
        |Node: `D100 = (D100 + 1)` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `D100 = 0` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `D100 = 0` -> Store: List(`D100` in `D100`, `n` (input))
        |Node: `D100` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `D100` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `D100` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `D100` -> Store: List(`n` (input))
        |Node: `R = (R + 1)` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = (D100 + 1)`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `R = 0` -> Store: List(`C1` in `C1 = 0`, `D100` in `D100 = 0`, `R` in `R`, `n` (input))
        |Node: `R` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = (D100 + 1)`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `R` -> Store: List(`C1` in `C1 = (C1 + 1)`, `D100` in `D100 = (D100 + 1)`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `R` -> Store: List(`C1` in `C1 = 0`, `D100` in `D100 = 0`, `n` (input))
        |Node: `i = (i + 1)` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `i` in `i = 0`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `i = 0` -> Store: List(`C1` in `C1 = 0`, `D100` in `D100 = 0`, `R` in `R = 0`, `i` in `i`, `n` (input))
        |Node: `i` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `i` in `i = 0`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `i` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `i` in `i = 0`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `i` -> Store: List(`C1` in `C1 = 0`, `D100` in `D100 = 0`, `R` in `R = 0`, `n` (input))
        |Node: `n` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `i` in `i = 0`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `tempPostfix#num0 = i` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`, `tempPostfix#num0` in `tempPostfix#num0`)
        |Node: `tempPostfix#num0` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `tempPostfix#num0` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`)
        |Node: `tempPostfix#num0` -> Store: List(`C1` in `C1 = (C1 + 1)`, `C1` in `C1 = 0`, `D100` in `D100 = (D100 + 1)`, `D100` in `D100 = 0`, `R` in `R = (R + 1)`, `R` in `R = 0`, `i` in `i = (i + 1)`, `n` (input), `tempPostfix#num0` in `tempPostfix#num0 = i`, `tempPostfix#num0` in `tempPostfix#num0`)""".stripMargin

    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", BoundCheckingUnitTest.test01, test01ExpectedOutput),
    )
  }
}