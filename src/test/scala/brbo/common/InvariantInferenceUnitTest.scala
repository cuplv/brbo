package brbo.common

import brbo.StringCompare
import brbo.boundinference.{BasicProcessor, BoundCheckingProcessor, BoundCheckingUnitTest}
import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta}
import brbo.common.InvariantInference.BeforeOrAfter.{AFTER, BEFORE}
import brbo.common.InvariantInference.Locations
import brbo.common.TypeUtils.BrboType.{BrboType, INT}
import com.sun.source.tree.MethodTree
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.node.Node
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashMap

class InvariantInferenceUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[BoundCheckingUnitTest])

  "Invariant inference for delta variable updates" should "succeed" in {
    BoundCheckingUnitTest.deltaVariableUpdateTests.foreach({
      testCase =>
        val basicProcessor = BasicProcessor.run(testCase.className, testCase.inputProgram)
        basicProcessor.assumeOneClassOneMethod()

        val methodTree: MethodTree = basicProcessor.getMethods.head._1
        val cfg = basicProcessor.getMethods.head._2
        val className = basicProcessor.getEnclosingClass(methodTree).get.getSimpleName.toString

        val solver = new Z3Solver
        val result = InvariantInference.inferInvariant(
          solver,
          className,
          methodTree,
          basicProcessor.getLineNumber,
          cfg,
          Locations(
            {
              node: Node =>
                GhostVariableUtils.extractGhostVariableUpdate(node, Delta) match {
                  case Some(_) => true
                  case None => false
                }
            },
            AFTER
          ),
          HashMap[String, BrboType](
            "R" -> INT,
            "n" -> INT,
            "m" -> INT,
            "l" -> INT,
            "C1" -> INT,
            "D100" -> INT
          )
        )
        logger.debug(result)
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput))
    })
  }

  "Invariant inference for delta variable resets" should "succeed" in {
    BoundCheckingUnitTest.deltaVariableResetTests.foreach({
      testCase =>
        val basicProcessor = BasicProcessor.run(testCase.className, testCase.inputProgram)
        basicProcessor.assumeOneClassOneMethod()

        val methodTree: MethodTree = basicProcessor.getMethods.head._1
        val cfg = basicProcessor.getMethods.head._2
        val className = basicProcessor.getEnclosingClass(methodTree).get.getSimpleName.toString

        val solver = new Z3Solver
        val result = InvariantInference.inferInvariant(
          solver,
          className,
          methodTree,
          basicProcessor.getLineNumber,
          cfg,
          Locations(
            {
              node: Node =>
                GhostVariableUtils.extractDeltaVariableReset(node) match {
                  case Some(_) => true
                  case None => false
                }
            },
            BEFORE
          ),
          HashMap[String, BrboType](
            "R" -> INT,
            "n" -> INT,
            "m" -> INT,
            "l" -> INT,
            "C1" -> INT,
            "D100" -> INT
          )
        )
        logger.debug(result)
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput))
    })
  }

  "Invariant inference for counter variable updates" should "succeed" in {
    BoundCheckingUnitTest.counterVariableUpdateTests.foreach({
      testCase =>
        val basicProcessor = BasicProcessor.run(testCase.className, testCase.inputProgram)
        basicProcessor.assumeOneClassOneMethod()

        val methodTree: MethodTree = basicProcessor.getMethods.head._1
        val cfg = basicProcessor.getMethods.head._2
        val className = basicProcessor.getEnclosingClass(methodTree).get.getSimpleName.toString

        val solver = new Z3Solver
        val result = InvariantInference.inferInvariant(
          solver,
          className,
          methodTree,
          basicProcessor.getLineNumber,
          cfg,
          Locations(
            {
              node: Node =>
                GhostVariableUtils.extractGhostVariableUpdate(node, Counter) match {
                  case Some(_) => true
                  case None => false
                }
            },
            AFTER
          ),
          HashMap[String, BrboType](
            "R" -> INT,
            "n" -> INT,
            "m" -> INT,
            "l" -> INT,
            "C1" -> INT,
            "D100" -> INT
          )
        )
        logger.debug(result)
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput))
    })
  }
}
