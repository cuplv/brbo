package brbo.boundinference.dependencyanalysis.reachingdefinition

import brbo.TestCaseJavaProgram
import brbo.boundinference.{BasicProcessor, BoundCheckingUnitTest}
import brbo.common.TreeUtils
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.analysis.{AnalysisResult, ForwardAnalysisImpl}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashSet

class ReachingDefinitionUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[ReachingDefinitionUnitTest])

  "Reaching definition analysis" should "be correct" in {
    val transfer = new ReachingTransfer()
    val forwardAnalysisImpl = new ForwardAnalysisImpl[ReachingValue, ReachingStore, ReachingTransfer](transfer)

    ReachingDefinitionUnitTest.reachingDefinitionUnitTest.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        forwardAnalysisImpl.performAnalysis(targetMethod.cfg)
        val result: AnalysisResult[ReachingValue, ReachingStore] = forwardAnalysisImpl.getResult
        TreeUtils.collectCommands(targetMethod.methodTree.getBody).foreach({
          statement =>
            println(s"Command: $statement -> Store: ${result.getStoreAfter(statement)}")
        })
    })
  }
}

object ReachingDefinitionUnitTest {
  val reachingDefinitionUnitTest: HashSet[TestCaseJavaProgram] = {
    HashSet[TestCaseJavaProgram](
      TestCaseJavaProgram("Test01", BoundCheckingUnitTest.test01, ""),
    )
  }
}