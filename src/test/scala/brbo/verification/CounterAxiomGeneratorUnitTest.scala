package brbo.verification

import brbo.StringCompare
import brbo.common.Z3Solver
import com.sun.source.tree.MethodTree
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

class CounterAxiomGeneratorUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[CounterAxiomGeneratorUnitTest])

  "Generating counter map" should "be correct" in {
    BoundCheckingUnitTest.counterGenerationTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val methodTree: MethodTree = targetMethod.methodTree
        val result = {
          val result = CounterAxiomGenerator.generateCounterMap(methodTree.getBody)
          result.toList.sortWith({
            case (pair1, pair2) => pair1._2 < pair2._2
          })
        }
        assert(StringCompare.ignoreWhitespaces(result.toString(), testCase.expectedOutput))
    })
  }

  "Generating counter axioms" should "output correct predicates" in {
    BoundCheckingUnitTest.counterAxiomsTests.foreach({
      testCase =>
        val targetMethod = BasicProcessor.getTargetMethod(testCase.className, testCase.inputProgram)
        val methodTree: MethodTree = targetMethod.methodTree
        val solver = new Z3Solver
        val result = CounterAxiomGenerator.generateCounterAxioms(solver, methodTree.getBody)
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput))
    })
  }
}
