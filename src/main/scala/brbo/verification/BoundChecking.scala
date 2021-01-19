package brbo.verification

import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta, Resource}
import brbo.common.InvariantInference.BeforeOrAfter.{AFTER, BEFORE}
import brbo.common.InvariantInference.Locations
import brbo.common.TreeUtils.collectCommands
import brbo.common.TypeUtils.BrboType.{BrboType, INT}
import brbo.common._
import brbo.verification.Decomposition.DecompositionResult
import com.microsoft.z3.{AST, Expr}
import com.sun.source.tree.{AssertTree, MethodTree}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.node.Node

object BoundChecking {
  private val logger = LogManager.getLogger("brbo.boundinference.BoundChecking")

  def checkBound(solver: Z3Solver,
                 decompositionResult: DecompositionResult,
                 boundExpression: AST,
                 printModelIfFail: Boolean): Boolean = {
    val targetMethod = decompositionResult.targetMethod
    val deltaCounterPairs = decompositionResult.deltaCounterPairs

    logger.info(s"Verifying bound $boundExpression in method ${targetMethod.methodTree.getName} of class $targetMethod.className")

    val methodBody = targetMethod.methodTree.getBody
    assert(methodBody != null)

    val inputVariables = targetMethod.inputVariables
    val localVariables = targetMethod.localVariables

    val resourceVariable: (String, BrboType) = {
      val resourceVariables = localVariables.filter({ case (identifier, _) => GhostVariableUtils.isGhostVariable(identifier, Resource) })
      assert(resourceVariables.size == 1)
      resourceVariables.head
    }
    logger.debug(s"Resource variable: $resourceVariable")

    /*val typeContext = {
      assert(inputVariables.keySet.intersect(localVariables.keySet).isEmpty)
      inputVariables ++ localVariables
    }

    val boundExpression = BoundChecking.extractBoundExpression(solver, methodTree, typeContext)*/

    val globalScopeVariables: Map[String, BrboType] = {
      val variables: Set[String] =
        deltaCounterPairs.map({ deltaCounterPair => deltaCounterPair.delta }) ++ // Delta variables
          CounterAxiomGenerator.generateCounterMap(methodBody).values + // Counters for all ASTs
          resourceVariable._1 // Resource variable
      variables.foldLeft(inputVariables)({
        (acc, variable) => acc + (variable -> INT)
      })
    }
    logger.info(s"For Z3, we declare these variables in the global scope: $globalScopeVariables")

    val invariantInference = new InvariantInference(targetMethod)
    val invariants: Set[(AST, AST, AST)] = deltaCounterPairs.map({
      deltaCounterPair =>
        val deltaVariable = deltaCounterPair.delta

        val peakInvariant = invariantInference.inferInvariant(
          solver,
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
          globalScopeVariables
        )
        logger.trace(s"Invariant for the peak value of delta variable $deltaVariable:\n$peakInvariant")

        val accumulationInvariant = {
          val accumulationInvariant = invariantInference.inferInvariant(
            solver,
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
            globalScopeVariables
          )

          // Delta variables' double primed version represents the maximum amount of accumulation per execution of subprograms
          val doublePrimeInvariant = accumulationInvariant.asInstanceOf[Expr].substitute(
            solver.mkIntVar(deltaVariable).asInstanceOf[Expr],
            solver.mkIntVar(generateDeltaVariableDoublePrime(deltaVariable)).asInstanceOf[Expr]
          )
          doublePrimeInvariant
        }
        logger.trace(s"Invariant for the accumulation of delta variable $deltaVariable (per visit to its subprogram):\n$accumulationInvariant")

        val counterInvariant = invariantInference.inferInvariant(
          solver,
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
          globalScopeVariables
        )
        logger.trace(s"Invariant for AST counter $deltaCounterPair:\n$counterInvariant")

        (peakInvariant, accumulationInvariant, counterInvariant)
    })

    val resourceInvariants: AST = {
      val resourceVariableUpperBound = {
        val items: Seq[AST] = deltaCounterPairs.map({
          deltaCounterPair =>
            val counterMinusOne = solver.mkSub(solver.mkIntVar(deltaCounterPair.counter), solver.mkIntVal(1))
            solver.mkAdd(
              solver.mkIntVar(deltaCounterPair.delta),
              solver.mkMul(
                solver.mkITE(
                  solver.mkGe(counterMinusOne, solver.mkIntVal(0)),
                  counterMinusOne,
                  solver.mkIntVal(0)
                ),
                solver.mkIntVar(generateDeltaVariableDoublePrime(deltaCounterPair.delta)))
            )
        }).toSeq
        solver.mkLe(
          solver.mkIntVar(resourceVariable._1),
          solver.mkAdd(items: _*)
        )
      }
      logger.info(s"Inductive invariant:\n$resourceVariableUpperBound")

      val deltaDoublePrimeInvariants = solver.mkAnd(invariants.map({ case (_, invariant, _) => invariant }).toSeq: _*)
      solver.mkAnd(resourceVariableUpperBound, deltaDoublePrimeInvariants)
    }

    val deltaInvariants = {
      solver.mkAnd(invariants.map({ case (invariant, _, _) => invariant }).toSeq: _*)
    }

    val counterInvariants = {
      val counterAxioms: AST = CounterAxiomGenerator.generateCounterAxioms(solver, methodBody)
      logger.debug(s"Counter axioms:\n$counterAxioms")

      val counterConstraints: AST = {
        val nonNegativeCounters =
          globalScopeVariables
            .filter({ case (identifier, _) => GhostVariableUtils.isGhostVariable(identifier, Counter) })
            .map({ case (counter, _) => solver.mkGe(solver.mkIntVar(counter), solver.mkIntVal(0)) }).toSeq
        solver.mkAnd(
          solver.mkLe(solver.mkIntVar(CounterAxiomGenerator.FIRST_COUNTER_NAME), solver.mkIntVal(1)),
          solver.mkAnd(nonNegativeCounters: _*)
        )
      }

      solver.mkAnd(
        counterAxioms,
        counterConstraints,
        solver.mkAnd(invariants.map({ case (_, _, invariant) => invariant }).toSeq: _*)
      )
    }

    // Sanity check: We assume the generated constraints won't contradict with each other
    checkSAT(solver, resourceInvariants)
    checkSAT(solver, deltaInvariants)
    checkSAT(solver, counterInvariants)
    checkSAT(solver, solver.mkAnd(resourceInvariants, deltaInvariants))
    checkSAT(solver, solver.mkAnd(resourceInvariants, counterInvariants))
    checkSAT(solver, solver.mkAnd(deltaInvariants, deltaInvariants))
    checkSAT(solver, solver.mkAnd(resourceInvariants, deltaInvariants, counterInvariants))

    checkSAT(solver, solver.mkNot(boundExpression))

    solver.mkAssert(resourceInvariants)
    solver.mkAssert(deltaInvariants)
    solver.mkAssert(counterInvariants)
    solver.mkAssert(solver.mkNot(boundExpression))
    val result = !solver.checkSAT(printUnsatCore = false)
    if (!result && printModelIfFail) {
      logger.fatal(s"Bound expression could not be verified: $boundExpression")
      solver.printAssertions()
      solver.printModel()
    }
    result
  }

  def checkSAT(solver: Z3Solver, ast: AST): Unit = {
    solver.push()
    solver.mkAssert(ast)
    assert(solver.checkSAT(printUnsatCore = true))
    solver.pop()
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

  private def generateDeltaVariableDoublePrime(identifier: String): String = {
    assert(GhostVariableUtils.isGhostVariable(identifier, Delta))
    s"$identifier\'\'"
  }

}
