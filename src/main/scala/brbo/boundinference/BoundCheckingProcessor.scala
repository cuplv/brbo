package brbo.boundinference

import brbo.boundinference.BoundCheckingProcessor.DeltaCounterPair
import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta}
import brbo.common.InvariantInference.BeforeOrAfter.{AFTER, BEFORE}
import brbo.common.InvariantInference.Locations
import brbo.common.TreeUtils.collectCommands
import brbo.common.TypeUtils.BrboType.BrboType
import brbo.common.{GhostVariableUtils, InvariantInference, TreeUtils, Z3Solver}
import com.microsoft.z3.AST
import com.sun.source.tree._
import javax.annotation.processing.SupportedAnnotationTypes
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.node.Node

import scala.collection.immutable.HashMap

@SupportedAnnotationTypes(Array("*"))
class BoundCheckingProcessor(solver: Z3Solver) extends BasicProcessor {
  private val logger = LogManager.getLogger(classOf[BoundCheckingProcessor])

  validateInputProgram()

  def checkBound(deltaCounterPairs: Set[DeltaCounterPair]): Boolean = {
    assumeOneClassOneMethod()

    val methodTree: MethodTree = getMethods.head._1
    val cfg = getMethods.head._2
    val className = getEnclosingClass(methodTree).get.getSimpleName.toString
    val methodBody = methodTree.getBody
    assert(methodBody != null)

    val inputVariables: Map[String, BrboType] = TreeUtils.getAllInputVariables(methodTree)

    val localVariables: Map[String, BrboType] = TreeUtils.getAllDeclaredVariables(methodBody)

    val typeContext = {
      assert(inputVariables.keySet.intersect(localVariables.keySet).isEmpty)
      inputVariables ++ localVariables
    }

    val boundExpression = extractBoundExpression(solver, methodTree, typeContext)

    val counterAxioms: AST = CounterAxiomGenerator.generateCounterAxioms(solver, methodBody)

    val invariants = deltaCounterPairs.map({
      deltaCounterPair =>
        val peakInvariant = InvariantInference.inferInvariant(
          solver,
          className,
          methodTree,
          getLineNumber,
          cfg,
          Locations(
            {
              node: Node =>
                GhostVariableUtils.extractGhostVariableUpdate(node, Delta) match {
                  case Some(update) => update.identifier == deltaCounterPair.deltaVariable
                  case None => false
                }
            },
            AFTER
          ),
          ???
        )

        val accumulateInvariant = InvariantInference.inferInvariant(
          solver,
          className,
          methodTree,
          getLineNumber,
          cfg,
          Locations(
            {
              node: Node =>
                GhostVariableUtils.extractDeltaVariableReset(node) match {
                  case Some(identifier) => identifier == deltaCounterPair.deltaVariable
                  case None => false
                }
            },
            BEFORE
          ),
          ???
        )

        val counterInvariant = InvariantInference.inferInvariant(
          solver,
          className,
          methodTree,
          getLineNumber,
          cfg,
          Locations(
            {
              node: Node =>
                GhostVariableUtils.extractGhostVariableUpdate(node, Counter) match {
                  case Some(update) => update.identifier == deltaCounterPair.counter
                  case None => false
                }
            },
            AFTER
          ),
          ???
        )

        (peakInvariant, accumulateInvariant, counterInvariant)
    })

    val inductiveInvariant = ???

    ???
  }

  def extractBoundExpression(solver: Z3Solver, methodTree: MethodTree, typeContext: Map[String, BrboType]): AST = {
    val methodBody = methodTree.getBody
    if (methodBody == null)
      throw new Exception(s"There is no method body to extract bound expression from in $methodTree")
    val commands = collectCommands(methodTree.getBody)
    val assertions = commands.filter(tree => tree.isInstanceOf[AssertTree])
    assert(assertions.size == 1, "Please use exact 1 assertion as the bound expression to be verified")
    TreeUtils.translatePureExpressionToZ3AST(solver, assertions.head.asInstanceOf[AssertTree].getCondition, typeContext)
  }

  /**
   * The input program of this processor should include:
   * 1. 1 reset for each delta variable
   * 2. >=1 updates for each delta variable
   * 3. 1 update for each counter
   */
  def validateInputProgram(): Unit = {
  }
}

object BoundCheckingProcessor {

  case class DeltaCounterPair(deltaVariable: String, counter: String)

}
