package brbo.boundinference

import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta, Resource}
import brbo.common.InvariantInference.BeforeOrAfter.{AFTER, BEFORE}
import brbo.common.InvariantInference.Locations
import brbo.common.TreeUtils.collectCommands
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import brbo.common.{GhostVariableUtils, InvariantInference, TreeUtils, Z3Solver}
import com.microsoft.z3.{AST, Expr}
import com.sun.source.tree.{AssertTree, MethodTree, Tree}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.node.Node

object BoundChecking {
  private val logger = LogManager.getLogger("brbo.boundinference.BoundChecking")

  def checkBound(solver: Z3Solver,
                 className: String,
                 methodTree: MethodTree,
                 getLineNumber: Tree => Int,
                 cfg: ControlFlowGraph,
                 deltaCounterPairs: Set[DeltaCounterPair],
                 boundExpression: AST): Boolean = {
    logger.info(s"Verifying bound $boundExpression in method ${methodTree.getName} of class $className")

    val methodBody = methodTree.getBody
    assert(methodBody != null)

    val inputVariables: Map[String, BrboType] = TreeUtils.getAllInputVariables(methodTree)
    logger.debug(s"Input variables: $inputVariables")

    val localVariables: Map[String, BrboType] = TreeUtils.getAllDeclaredVariables(methodBody)
    logger.debug(s"Local variables: $localVariables")

    val resourceVariable = {
      val resourceVariables = localVariables.filter({ case (identifier, _) => GhostVariableUtils.isGhostVariable(identifier, Resource) })
      assert(resourceVariables.size == 1)
      resourceVariables.head
    }
    logger.debug(s"Resource variable: $resourceVariable")

    val typeContext = {
      assert(inputVariables.keySet.intersect(localVariables.keySet).isEmpty)
      inputVariables ++ localVariables
    }

    // val boundExpression = BoundChecking.extractBoundExpression(solver, methodTree, typeContext)

    val counterAxioms: AST = CounterAxiomGenerator.generateCounterAxioms(solver, methodBody)
    logger.debug(s"Counter axioms: $counterAxioms")

    val invariants: Seq[AST] = deltaCounterPairs.map({
      deltaCounterPair =>
        val deltaVariable = deltaCounterPair.delta

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
                  case Some(update) => update.identifier == deltaVariable
                  case None => false
                }
            },
            AFTER
          ),
          localVariables - deltaVariable
        )
        logger.trace(s"Invariant for the peak value of delta variable $deltaVariable is $peakInvariant")

        val accumulationInvariant = {
          val accumulationInvariant = InvariantInference.inferInvariant(
            solver,
            className,
            methodTree,
            getLineNumber,
            cfg,
            Locations(
              {
                node: Node =>
                  GhostVariableUtils.extractDeltaVariableReset(node) match {
                    case Some(identifier) => identifier == deltaVariable
                    case None => false
                  }
              },
              BEFORE
            ),
            localVariables - deltaVariable
          )
          accumulationInvariant.asInstanceOf[Expr].substitute(
            solver.mkIntVar(deltaVariable).asInstanceOf[Expr],
            solver.mkIntVar(generateDeltaVariablePrime(deltaVariable)).asInstanceOf[Expr]
          )
        }
        logger.trace(s"Invariant for the accumulation of delta variable $deltaVariable (per visit to its subprogram) is $accumulationInvariant")

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
          localVariables - deltaCounterPair.counter
        )
        logger.trace(s"Invariant for AST counter $deltaCounterPair is $counterInvariant")

        solver.mkAnd(peakInvariant, accumulationInvariant, counterInvariant)
    }).toSeq

    val inductiveInvariant: AST = {
      val summands: Seq[AST] = deltaCounterPairs.map({
        deltaCounterPair =>
          solver.mkAdd(
            solver.mkIntVar(deltaCounterPair.delta),
            solver.mkMul(
              solver.mkIntVar(deltaCounterPair.counter),
              solver.mkIntVar(generateDeltaVariablePrime(deltaCounterPair.delta)))
          )
      }).toSeq
      solver.mkLe(
        solver.mkIntVar(resourceVariable._1),
        solver.mkAdd(summands: _*)
      )
    }
    logger.info(s"Inductive invariant is $inductiveInvariant")

    val universallyQuantify: Set[AST] = {
      val set1 = inputVariables.map({
        case (identifier, typ) =>
          typ match {
            case INT => solver.mkIntVar(identifier)
            case BOOL => solver.mkBoolVar(identifier)
          }
      }).toSet
      val set2 = deltaCounterPairs.map({ deltaCounterPair => solver.mkIntVar(deltaCounterPair.delta) })
      val set3 = deltaCounterPairs.map({ deltaCounterPair => solver.mkIntVar(deltaCounterPair.counter) })
      set1 ++ set2 ++ set3 + solver.mkIntVar(resourceVariable._1)
    }
    logger.debug(s"Inductive invariant is universally quantified by $universallyQuantify")

    val query = solver.mkForall(
      universallyQuantify,
      solver.mkImplies(
        solver.mkAnd(
          inductiveInvariant,
          solver.mkAnd(invariants: _*),
          counterAxioms
        ),
        boundExpression
      )
    )
    solver.mkAssert(query)
    solver.checkSAT
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

  case class DeltaCounterPair(delta: String, counter: String)

  private def generateDeltaVariablePrime(identifier: String): String = {
    assert(GhostVariableUtils.isGhostVariable(identifier, Delta))
    s"${identifier}000"
  }

}
