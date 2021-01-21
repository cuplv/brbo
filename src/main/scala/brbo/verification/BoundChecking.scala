package brbo.verification

import brbo.BrboMain
import brbo.common.BeforeOrAfterOrThis.{AFTER, BEFORE, THIS}
import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta, Resource}
import brbo.common.InstrumentUtils.FileFormat.JAVA_FORMAT
import brbo.common.InstrumentUtils.{NewMethodInformation, StatementTreeInstrumentation}
import brbo.common.TreeUtils.collectCommands
import brbo.common.TypeUtils.BrboType.{BrboType, INT}
import brbo.common.{Locations, _}
import brbo.verification.AmortizationMode.SELECTIVE_AMORTIZE
import brbo.verification.Decomposition.{DecompositionResult, DeltaCounterPair}
import com.microsoft.z3.{AST, Expr}
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager

import scala.collection.immutable.HashSet

object BoundChecking {
  private val logger = LogManager.getLogger("brbo.verification.BoundChecking")

  def treatCounterAsResourceInstrumentation(counterVariable: String, resourceVariable: String): StatementTreeInstrumentation = {
    StatementTreeInstrumentation(
      Locations(shouldInstrument, THIS),
      whatToInsert(counterVariable, resourceVariable)
    )
  }

  private def shouldInstrument(tree: StatementTree): Boolean = {
    if (tree == null) return false

    if (TreeUtils.isCommand(tree)) {
      tree match {
        case variableTree: VariableTree => GhostVariableUtils.isGhostVariable(variableTree.getName.toString)
        case expressionStatementTree: ExpressionStatementTree =>
          GhostVariableUtils.isReset(expressionStatementTree.getExpression) || GhostVariableUtils.isUpdate(expressionStatementTree.getExpression)
        case _: AssertTree => true
        case _ => false
      }
    }
    else false
  }

  private def whatToInsert(counterVariable: String, resourceVariable: String)(tree: StatementTree): String = {
    if (tree == null) return ""

    if (TreeUtils.isCommand(tree)) {
      val originalCommand = s"${tree.toString}"
      tree match {
        case variableTree: VariableTree =>
          val variableName = variableTree.getName.toString
          if (GhostVariableUtils.isGhostVariable(variableName)) {
            if (variableName != counterVariable) ""
            else {
              s"int $resourceVariable = 0;"
            }
          }
          else originalCommand
        case expressionStatementTree: ExpressionStatementTree =>
          val expression = expressionStatementTree.getExpression
          (GhostVariableUtils.isUpdate(expression), GhostVariableUtils.isReset(expression)) match {
            case (true, true) => throw new Exception("Unreachable")
            case (true, false) =>
              expression match {
                case assignmentTree: AssignmentTree =>
                  val lhs = assignmentTree.getVariable.toString
                  if (lhs == counterVariable) {
                    GhostVariableUtils.extractUpdate(expression, Counter) match {
                      case Some(updateTree) =>
                        assert(updateTree.identifier == counterVariable)
                        s"$resourceVariable = $resourceVariable + ${updateTree.increment.toString};"
                      case None => throw new Exception("Unreachable")
                    }
                  }
                  else ""
                case _ => throw new Exception("Unreachable")
              }
            case (false, true) =>
              expression match {
                case assignmentTree: AssignmentTree =>
                  val lhs = assignmentTree.getVariable.toString
                  if (lhs == counterVariable) {
                    GhostVariableUtils.extractReset(expression, Counter) match {
                      case Some(variableName) =>
                        assert(variableName == counterVariable)
                        s"$resourceVariable = 0"
                      case None => throw new Exception("Unreachable")
                    }
                  }
                  else ""
                case _ => throw new Exception("Unreachable")
              }
            case (false, false) => originalCommand
          }
        case _: AssertTree => ""
        case _ => originalCommand
      }
    }
    else throw new Exception("Unreachable")
  }

  def inferInvariantsForResource(solver: Z3Solver, decompositionResult: DecompositionResult, commandLineArguments: CommandLineArguments): GlobalInvariants = {
    val deltaCounterPairs = decompositionResult.deltaCounterPairs

    val methodBody = decompositionResult.outputMethod.methodTree.getBody
    assert(methodBody != null)

    val inputVariables = decompositionResult.outputMethod.inputVariables
    val localVariables = decompositionResult.outputMethod.localVariables

    val resourceVariable: (String, BrboType) = {
      val resourceVariables = localVariables.filter({ case (identifier, _) => GhostVariableUtils.isGhostVariable(identifier, Resource) })
      assert(resourceVariables.size == 1, s"There must be exactly 1 resource variable. Instead we have `$resourceVariables`")
      resourceVariables.head
    }
    logger.info(s"Resource variable: $resourceVariable")

    val globalScopeVariables: Map[String, BrboType] = {
      val variables: Set[String] =
        deltaCounterPairs.map({ deltaCounterPair => deltaCounterPair.delta }) ++ // Delta variables
          CounterAxiomGenerator.generateCounterMap(methodBody).values + // Counters for all ASTs
          resourceVariable._1 // Resource variable
      variables.foldLeft(inputVariables)({
        (acc, variable) => acc + (variable -> INT)
      })
    }
    logger.trace(s"For Z3, we declare these variables in the global scope: `$globalScopeVariables`")

    logger.info(s"No matter what is the specified amortization mode, we infer invariants for counters with `$SELECTIVE_AMORTIZE`")
    val newCommandLineArguments = CommandLineArguments(SELECTIVE_AMORTIZE, commandLineArguments.debugMode, commandLineArguments.directoryToAnalyze)
    val invariantInference = new InvariantInference(decompositionResult.outputMethod)
    val invariants: Set[(AST, AST, AST)] = deltaCounterPairs.map({
      deltaCounterPair =>
        val deltaVariable = deltaCounterPair.delta
        val counterVariable = deltaCounterPair.counter

        logger.info(s"Infer invariant for the peak value of delta variable `$deltaVariable`")
        val peakInvariant = invariantInference.inferInvariant(
          solver,
          Locations(
            {
              case expressionStatementTree: ExpressionStatementTree =>
                GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Delta) match {
                  case Some(update) => update.identifier == deltaVariable
                  case None => false
                }
              case _ => false
            },
            AFTER
          ),
          localVariables - deltaVariable,
          globalScopeVariables
        )

        logger.info(s"Infer invariant for the accumulation of delta variable `$deltaVariable` (per visit to its subprogram)")
        val accumulationInvariant = {
          val accumulationInvariant = invariantInference.inferInvariant(
            solver,
            Locations(
              {
                case expressionStatementTree: ExpressionStatementTree =>
                  GhostVariableUtils.extractReset(expressionStatementTree.getExpression, Delta) match {
                    case Some(identifier) => identifier == deltaVariable
                    case None => false
                  }
                case _ => false
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

        val isCounterUpdateInLoop: Boolean = {
          decompositionResult.outputMethod.commands.find({
            case expressionStatementTree: ExpressionStatementTree =>
              GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Counter) match {
                case Some(updateTree) => updateTree.identifier == counterVariable
                case None => false
              }
            case _ => false
          }) match {
            case Some(command) =>
              TreeUtils.getMinimalEnclosingLoop(decompositionResult.outputMethod.getPath(command)) match {
                case Some(_) => true
                case None => false
              }
            case None => throw new Exception("Unreachable")
          }
        }

        val counterInvariant: Expr = if (isCounterUpdateInLoop) {
          logger.info(s"Infer invariants for AST counter `$counterVariable` by treating it as consuming resources")
          val newResourceVariable = GhostVariableUtils.generateName(HashSet[String](resourceVariable._1), Resource)
          val newMethodBody = InstrumentUtils.instrumentStatementTrees(
            decompositionResult.outputMethod,
            treatCounterAsResourceInstrumentation(counterVariable, newResourceVariable),
            indent = 0
          )
          val newSourceFileContents = InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(
            decompositionResult.outputMethod,
            NewMethodInformation(None, None, None, List("import brbo.benchmarks.Common;"), Some("Common"), isAbstractClass = true, newMethodBody),
            JAVA_FORMAT,
            indent = 2
          )
          val sourceFilePath = s"${decompositionResult.outputMethod.fullQualifiedClassName}.java"
          BrboMain.inferResourceInvariants(solver, sourceFilePath, newSourceFileContents, newCommandLineArguments) match {
            case Some(globalInvariant) =>
              val invariant = solver.mkAnd(globalInvariant.resourceInvariants, globalInvariant.deltaInvariants, globalInvariant.counterInvariants)
              val existentiallyQuantify: Set[Expr] = globalInvariant.deltaCounterPairs
                .flatMap(pair => HashSet[String](pair.delta, pair.counter))
                .map(identifier => solver.mkIntVar(identifier))
              val invariant2 = solver.mkExists(existentiallyQuantify, invariant)
              val invariant3 = invariant2.substitute(solver.mkIntVar(newResourceVariable), solver.mkIntVar(counterVariable))
              invariant3
            case None => solver.mkTrue()
          }
        }
        else {
          logger.info(s"Infer invariants for AST counter `$counterVariable` with ICRA")
          invariantInference.inferInvariant(
            solver,
            Locations(
              {
                case expressionStatementTree: ExpressionStatementTree =>
                  GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Counter) match {
                    case Some(update) => update.identifier == counterVariable
                    case None => false
                  }
                case _ => false
              },
              AFTER
            ),
            localVariables - counterVariable,
            globalScopeVariables
          )
        }

        (peakInvariant, accumulationInvariant, counterInvariant)
    })

    if (invariants.isEmpty) logger.fatal(s"No invariant was inferred by ICRA!")

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
      logger.trace(s"Counter axioms:\n$counterAxioms")

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
    logger.info(s"Sanity check finished")

    GlobalInvariants(resourceInvariants, deltaInvariants, counterInvariants, deltaCounterPairs)
  }

  def checkBound(solver: Z3Solver,
                 decompositionResult: DecompositionResult,
                 boundExpression: AST,
                 printModelIfFail: Boolean,
                 commandLineArguments: CommandLineArguments): Boolean = {
    logger.info("")
    logger.info("")
    logger.info(s"Checking bound... Mode: `${decompositionResult.amortizationMode}`")

    val targetMethod = decompositionResult.outputMethod
    logger.info(s"Verify bound `$boundExpression` in method `${targetMethod.methodTree.getName}` of class `${targetMethod.fullQualifiedClassName}`")

    checkSAT(solver, solver.mkNot(boundExpression))
    logger.info(s"Sanity check finished")

    val globalInvariants = inferInvariantsForResource(solver, decompositionResult, commandLineArguments)
    solver.mkAssert(globalInvariants.resourceInvariants)
    solver.mkAssert(globalInvariants.deltaInvariants)
    solver.mkAssert(globalInvariants.counterInvariants)
    solver.mkAssert(solver.mkNot(boundExpression))
    val result: Boolean = {
      try {
        val result = !solver.checkSAT(printUnsatCore = false)
        if (!result && printModelIfFail && decompositionResult.amortizationMode == SELECTIVE_AMORTIZE) {
          // solver.printAssertions()
          solver.printModel()
        }
        result
      }
      catch {
        case e: Z3TimeoutException =>
          logger.fatal(s"Exception when running Z3: ${e.message}")
          false
        case e: Exception =>
          logger.fatal(s"Unknown exception when running Z3: ${e.getMessage}")
          false
      }
    }
    val yesOrNo = if (result) "Yes!" else "No!"
    logger.info(s"Is bound `$boundExpression` verified? $yesOrNo (Mode: `${decompositionResult.amortizationMode}`; Class `${decompositionResult.inputMethod.fullQualifiedClassName}`)")
    result
  }

  private def checkSAT(solver: Z3Solver, ast: AST): Unit = {
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

  def extractBoundAndCheck(decompositionResult: DecompositionResult, commandLineArguments: CommandLineArguments): Unit = {
    val inputMethod = decompositionResult.inputMethod
    val solver: Z3Solver = new Z3Solver
    val boundExpression: AST = BoundChecking.extractBoundExpression(solver, inputMethod.methodTree, inputMethod.inputVariables ++ inputMethod.localVariables)
    logger.info(s"Extracted bound expression is `$boundExpression`")
    checkBound(solver, decompositionResult, boundExpression, printModelIfFail = true, commandLineArguments)
  }

  case class GlobalInvariants(resourceInvariants: AST, deltaInvariants: AST, counterInvariants: AST, deltaCounterPairs: Set[DeltaCounterPair])

}
