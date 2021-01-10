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

    val universallyQuantify: Map[String, BrboType] = {
      val variables: Set[String] =
        deltaCounterPairs.map({ deltaCounterPair => deltaCounterPair.delta }) ++ // Delta variables
          deltaCounterPairs.map({ deltaCounterPair => generateDeltaVariablePrime(deltaCounterPair.delta) }) ++ // Delta variables' primed version
          deltaCounterPairs.map({ deltaCounterPair => deltaCounterPair.counter }) ++ // Counters for some ASTs
          CounterAxiomGenerator.generateCounterMap(methodBody).values + // Counters for all ASTs
          resourceVariable._1 // Resource variable
      variables.foldLeft(inputVariables)({
        (acc, variable) => acc + (variable -> INT)
      })
    }
    logger.info(s"Inductive invariant is universally quantified by $universallyQuantify")

    val universallyQuantifyASTs: List[AST] =
      universallyQuantify
        .toList.sortWith({ case (pair1, pair2) => pair1._2 < pair2._2 })
        .map({
          case (identifier, typ) =>
            typ match {
              case INT => solver.mkIntVar(identifier)
              case BOOL => solver.mkBoolVar(identifier)
            }
        })

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
            universallyQuantify
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
          universallyQuantify
        )
        logger.trace(s"Invariant for AST counter $deltaCounterPair is $counterInvariant")

        solver.mkAnd(peakInvariant, accumulationInvariant, counterInvariant)
    }).toSeq

    val resourceVariableUpperBound: AST = {
      val resourceVariableUpperBound = {
        val summands: Seq[AST] = deltaCounterPairs.map({
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
          solver.mkAdd(summands: _*)
        )
      }
      logger.info(s"Inductive invariant is $resourceVariableUpperBound")

      val maximumAccumulation = {
        val greaterThanOrEqualTo = deltaCounterPairs.map({
          deltaCounterPair =>
            val prime = generateDeltaVariablePrime(deltaCounterPair.delta)
            val doublePrime = generateDeltaVariableDoublePrime(deltaCounterPair.delta)
            solver.mkGe(solver.mkIntVar(doublePrime), solver.mkIntVar(prime))
        }).toSeq
        solver.mkAnd(greaterThanOrEqualTo: _*)
      }

      // Delta variables' double primed version represents the maximum amount of accumulation per execution of subprograms
      val existentiallyQuantify = deltaCounterPairs
        .map({ deltaCounterPair => generateDeltaVariableDoublePrime(deltaCounterPair.delta) })
        .map(identifier => solver.mkIntVar(identifier))

      solver.mkExists(existentiallyQuantify, solver.mkAnd(resourceVariableUpperBound, maximumAccumulation))
    }

    val implication =
      solver.mkImplies(
        solver.mkAnd(
          resourceVariableUpperBound,
          solver.mkAnd(invariants: _*),
          counterAxioms,
          counterConstraints
        ),
        boundExpression
      )
    val query = solver.mkForall(universallyQuantifyASTs, implication)
    println(implication)
    println(query)
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
    s"${identifier}\'"
  }

  private def generateDeltaVariableDoublePrime(identifier: String): String = {
    assert(GhostVariableUtils.isGhostVariable(identifier, Delta))
    s"${identifier}\'\'"
  }

}
