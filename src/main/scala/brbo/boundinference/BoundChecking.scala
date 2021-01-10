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

    val resourceVariable: (String, BrboType) = {
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

    val universallyQuantify: Map[String, BrboType] = {
      val variables: Set[String] =
        deltaCounterPairs.map({ deltaCounterPair => deltaCounterPair.delta }) ++ // Delta variables
          deltaCounterPairs.map({ deltaCounterPair => generateDeltaVariablePrime(deltaCounterPair.delta) }) ++ // Delta variables' primed version
          CounterAxiomGenerator.generateCounterMap(methodBody).values + // Counters for all ASTs
          resourceVariable._1 // Resource variable
      variables.foldLeft(inputVariables)({
        (acc, variable) => acc + (variable -> INT)
      })
    }
    logger.info(s"Inductive invariant is universally quantified by $universallyQuantify")

    val counterConstraints: AST = {
      val nonNegativeCounters =
        universallyQuantify
          .filter({ case (identifier, _) => GhostVariableUtils.isGhostVariable(identifier, Counter) })
          .map({ case (counter, _) => solver.mkGe(solver.mkIntVar(counter), solver.mkIntVal(0)) }).toSeq
      solver.mkAnd(
        solver.mkLe(solver.mkIntVar(CounterAxiomGenerator.FIRST_COUNTER_NAME), solver.mkIntVal(1)),
        solver.mkAnd(nonNegativeCounters: _*)
      )
    }

    val invariants: Set[(AST, AST, AST)] = deltaCounterPairs.map({
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
          localVariables - deltaVariable,
          universallyQuantify
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
            localVariables - deltaVariable,
            universallyQuantify
          )

          // Delta variables' double primed version represents the maximum amount of accumulation per execution of subprograms
          val doublePrimeInvariant = accumulationInvariant.asInstanceOf[Expr].substitute(
            solver.mkIntVar(deltaVariable).asInstanceOf[Expr],
            solver.mkIntVar(generateDeltaVariableDoublePrime(deltaVariable)).asInstanceOf[Expr]
          )
          doublePrimeInvariant
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
          localVariables - deltaCounterPair.counter,
          universallyQuantify
        )
        logger.trace(s"Invariant for AST counter $deltaCounterPair is $counterInvariant")

        (peakInvariant, accumulationInvariant, counterInvariant)
    })

    val resourceVariableUpperBound: AST = {
      val resourceVariableUpperBound = {
        val items: Seq[AST] = deltaCounterPairs.map({
          deltaCounterPair =>
            solver.mkAdd(
              solver.mkIntVar(deltaCounterPair.delta),
              solver.mkMul(
                solver.mkIntVar(deltaCounterPair.counter),
                solver.mkIntVar(generateDeltaVariableDoublePrime(deltaCounterPair.delta)))
            )
        }).toSeq
        solver.mkLe(
          solver.mkIntVar(resourceVariable._1),
          solver.mkAdd(items: _*)
        )
      }
      logger.info(s"Inductive invariant is $resourceVariableUpperBound")

      val deltaDoublePrimeInvariants = solver.mkAnd(invariants.map({ case (_, invariant, _) => invariant }).toSeq: _*)
      solver.mkAnd(resourceVariableUpperBound, deltaDoublePrimeInvariants)
    }

    val deltaInvariants = {
      solver.mkAnd(invariants.map({ case (invariant, _, _) => invariant }).toSeq: _*)
    }

    val counterInvariants = {
      solver.mkAnd(
        counterAxioms,
        counterConstraints,
        solver.mkAnd(invariants.map({ case (_, _, invariant) => invariant }).toSeq: _*)
      )
    }

    solver.mkAssert(resourceVariableUpperBound)
    assert(solver.checkSAT(true))

    solver.mkAssert(deltaInvariants)
    assert(solver.checkSAT(true))

    solver.mkAssert(counterInvariants)
    assert(solver.checkSAT(true))

    solver.mkAssert(solver.mkNot(boundExpression))
    !solver.checkSAT(true)
  }

  def printVariableDeclarations(): String = {
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

  case class DeltaCounterPair(delta: String, counter: String)

  private def generateDeltaVariablePrime(identifier: String): String = {
    assert(GhostVariableUtils.isGhostVariable(identifier, Delta))
    s"$identifier\'"
  }

  private def generateDeltaVariableDoublePrime(identifier: String): String = {
    assert(GhostVariableUtils.isGhostVariable(identifier, Delta))
    s"$identifier\'\'"
  }

}
