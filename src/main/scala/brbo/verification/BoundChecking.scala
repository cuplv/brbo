package brbo.verification

import brbo.common.BeforeOrAfterOrThis.{AFTER, BEFORE, THIS}
import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta, Resource}
import brbo.common.InstrumentUtils.StatementTreeInstrumentation
import brbo.common.TreeUtils.collectCommands
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT, VOID}
import brbo.common.{Locations, _}
import brbo.verification.AmortizationMode.SELECTIVE_AMORTIZE
import brbo.verification.Decomposition.{DecompositionResult, DeltaCounterPair}
import com.microsoft.z3.{AST, Expr}
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

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

  def inferInvariantsForResource(solver: Z3Solver, decompositionResult: DecompositionResult, arguments: CommandLineArguments): GlobalInvariants = {
    def createVar(pair: (String, BrboType)): Expr = {
      pair._2 match {
        case INT => solver.mkIntVar(pair._1)
        case BOOL => solver.mkBoolVar(pair._1)
        case VOID => throw new Exception("Unexpected")
      }
    }

    val deltaCounterPairs = decompositionResult.deltaCounterPairs

    val methodBody = decompositionResult.outputMethod.methodTree.getBody
    assert(methodBody != null)

    val inputVariables: Map[String, BrboType] = decompositionResult.outputMethod.inputVariables

    val resourceVariable: (String, BrboType) = {
      val resourceVariables = decompositionResult.outputMethod.localVariables.filter({ case (identifier, _) => GhostVariableUtils.isGhostVariable(identifier, Resource) })
      assert(resourceVariables.size == 1, s"There must be exactly 1 resource variable. Instead we have `$resourceVariables`")
      resourceVariables.head
    }
    logger.info(s"Resource variable: $resourceVariable")

    val deltaVariables: Map[String, BrboType] =
      deltaCounterPairs.map({ deltaCounterPair => deltaCounterPair.delta }).foldLeft(new HashMap[String, BrboType])({
        (acc, delta) => acc + (delta -> INT)
      })
    val counterVariables: Map[String, BrboType] =
      CounterAxiomGenerator.generateCounterMap(methodBody).values.foldLeft(new HashMap[String, BrboType])({
        (acc, counter) => acc + (counter -> INT)
      })
    val localVariables: Map[String, BrboType] = decompositionResult.outputMethod.localVariables ++ counterVariables ++ deltaVariables
    val allVariables: Map[String, BrboType] = inputVariables ++ localVariables

    val lastTree = decompositionResult.outputMethod.methodTree.getBody.getStatements.asScala.last
    val invariantInference = new InvariantInference(decompositionResult.outputMethod)
    val invariants: Set[(AST, AST, AST)] = deltaCounterPairs.map({
      deltaCounterPair =>
        val deltaVariable = deltaCounterPair.delta
        val counterVariable = deltaCounterPair.counter

        logger.info(s"Infer invariant for the peak value of delta variable `$deltaVariable`")
        val globalInvariantFuture = Future {
          val invariant = invariantInference.inferInvariant(
            solver,
            Locations(
              {
                /*case expressionStatementTree: ExpressionStatementTree =>
                  val isUpdate = GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Delta) match {
                    case Some(update) => update.identifier == deltaVariable
                    case None => false
                  }
                  val isReset = GhostVariableUtils.extractReset(expressionStatementTree.getExpression, Delta) match {
                    case Some(identifier) => identifier == deltaVariable
                    case None => false
                  }
                  isUpdate || isReset
                case variableTree: VariableTree =>
                  if (variableTree.getName.toString == deltaVariable) true
                  else false*/
                case _: VariableTree => false // To ensure `D` is declared before each `assert(D==D)`
                case tree if TreeUtils.isCommand(tree) => true
                case _ => false
              },
              AFTER
            ),
            deltaVariable,
            allVariables
          )
          solver.mkExists(
            (localVariables - deltaVariable).map(pair => createVar(pair)),
            invariant
          )
        }

        // TODO: It seems that ICRA cannot infer strong invariants right before `D=0`
        logger.info(s"Infer invariant for the accumulation of delta variable `$deltaVariable` (per visit to its subprogram)")
        val accumulationInvariantFuture = Future {
          invariantInference.inferInvariant(
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
            deltaVariable,
            allVariables
          )
        }

        /*val isCounterUpdateInLoop: Boolean = {
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
        }*/

        val counterInvariantFuture = Future {
          /*if (isCounterUpdateInLoop) {
            logger.info(s"Infer invariants for AST counter `$counterVariable` by treating it as consuming resources (Mode: `$SELECTIVE_AMORTIZE`)")
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
          else {*/
          logger.info(s"Infer invariants for AST counter `$counterVariable` with ICRA")
          val invariant = invariantInference.inferInvariant(
            solver,
            Locations(
              {
                tree: Tree => if (tree == lastTree) true else false
              },
              AFTER
            ),
            counterVariable,
            allVariables
          )
          solver.mkExists(
            (localVariables - counterVariable).map(pair => createVar(pair)),
            invariant
          )
          //}
        }

        val globalInvariant = Await.result(globalInvariantFuture, Duration.Inf)
        val accumulationInvariant = Await.result(accumulationInvariantFuture, Duration.Inf)
        val counterInvariant = Await.result(counterInvariantFuture, Duration.Inf)

        // Delta variables' double primed version represents the maximum amount of accumulation per execution of subprograms
        val accumulationInvariantDoublePrime = {
          val accumulationConstraint = solver.mkExists(
            (localVariables - deltaVariable).map(pair => createVar(pair)),
            accumulationInvariant.substitute(
              solver.mkIntVar(deltaVariable),
              solver.mkIntVar(generateDeltaVariableDoublePrime(deltaVariable))
            )
          )
          val maxConstraint = solver.mkForall(
            List(solver.mkIntVar(deltaVariable)),
            solver.mkImplies(
              solver.mkExists(
                (localVariables - deltaVariable).map(pair => createVar(pair)),
                accumulationInvariant
              ),
              solver.mkGe(
                solver.mkIntVar(generateDeltaVariableDoublePrime(deltaVariable)),
                solver.mkIntVar(deltaVariable),
              )
            )
          )
          // solver.mkAnd(accumulationConstraint, maxConstraint)
          solver.mkAnd(
            accumulationConstraint,
            // Ensure the total accumulation should satisfy global invariants
            globalInvariant.substitute(
              solver.mkIntVar(deltaVariable),
              solver.mkIntVar(generateDeltaVariableDoublePrime(deltaVariable))
            )
          )
        }

        (globalInvariant, accumulationInvariantDoublePrime, counterInvariant)
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
                solver.mkIntVar(generateDeltaVariableDoublePrime(deltaCounterPair.delta))
              )
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
      val counterAxioms: AST = {
        logger.info(s"Provide counter axioms only for mode `$SELECTIVE_AMORTIZE` (Current mode: `${arguments.amortizationMode}`)")
        arguments.amortizationMode match {
          case SELECTIVE_AMORTIZE => CounterAxiomGenerator.generateCounterAxioms(solver, methodBody)
          case _ => solver.mkTrue()
        }
      }
      logger.trace(s"Counter axioms:\n$counterAxioms")

      val counterConstraints: AST = {
        val nonNegativeCounters = counterVariables.map({ case (counter, _) => solver.mkGe(solver.mkIntVar(counter), solver.mkIntVal(0)) }).toSeq
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

    GlobalInvariants(resourceInvariants, deltaInvariants, counterInvariants, deltaCounterPairs)
  }

  def checkBound(solver: Z3Solver,
                 decompositionResult: DecompositionResult,
                 boundExpression: AST,
                 arguments: CommandLineArguments): Boolean = {
    logger.info(s"Check bound (Mode: `${decompositionResult.amortizationMode}`)")

    val targetMethod = decompositionResult.outputMethod
    logger.info(s"Verify bound `$boundExpression` in method `${targetMethod.methodTree.getName}` of class `${targetMethod.fullQualifiedClassName}`")

    val globalInvariants = inferInvariantsForResource(solver, decompositionResult, arguments)

    val resourceInvariants = globalInvariants.resourceInvariants
    val deltaInvariants = globalInvariants.deltaInvariants
    val counterInvariants = globalInvariants.counterInvariants

    // Sanity check: We assume the generated constraints won't contradict with each other
    val checks = List[(AST, String)](
      (resourceInvariants, s"Sanity check - Invariants about resource variables should SAT"),
      (deltaInvariants, s"Sanity check - Invariants about delta variables should SAT"),
      (counterInvariants, s"Sanity check - Invariants about counter variables should SAT"),
      (solver.mkAnd(resourceInvariants, deltaInvariants), s"Sanity check - Invariants about resource and delta variables should SAT"),
      (solver.mkAnd(resourceInvariants, counterInvariants), s"Sanity check - Invariants about resource and counter variables should SAT"),
      (solver.mkAnd(deltaInvariants, counterInvariants), s"Sanity check - Invariants about delta and counter variables should SAT"),
      (solver.mkAnd(resourceInvariants, deltaInvariants, counterInvariants), s"Sanity check - Invariants about resource, delta, and counter variables should SAT"),
      (boundExpression, s"Sanity check - The bound expression should SAT"),
      (solver.mkAnd(resourceInvariants, deltaInvariants, boundExpression), "1"),
      (solver.mkAnd(resourceInvariants, counterInvariants, boundExpression), "2"),
      (solver.mkAnd(deltaInvariants, counterInvariants, boundExpression), "3")
    )
    sanityCheck(solver, checks, expect = true, allowFail = true, arguments.skipSanityCheck)
    logger.info(s"Sanity check finished")

    solver.mkAssert(resourceInvariants)
    solver.mkAssert(deltaInvariants)
    solver.mkAssert(counterInvariants)
    solver.mkAssert(solver.mkNot(boundExpression))
    val result: Boolean = {
      try {
        logger.info(s"Discharge bound check query to Z3")
        val result = !solver.checkSAT(printUnsatCore = false)
        if (!result) {
          if (arguments.debugMode || decompositionResult.amortizationMode == SELECTIVE_AMORTIZE || arguments.printCounterExample) {
            // solver.printAssertions()
            solver.printModel()
          }
        }
        result
      }
      catch {
        case e: Z3UnknownException =>
          logger.fatal(s"Bound check - Z3 returns UNKNOWN: ${e.message}")
          false
        case e: Exception =>
          logger.fatal(s"Bound check - Unknown exception when running Z3: ${e.getMessage}")
          false
      }
    }
    val yesOrNo = if (result) "Yes!" else "No!"
    logger.info(s"Is bound `$boundExpression` verified? $yesOrNo (Mode: `${decompositionResult.amortizationMode}`; Class `${decompositionResult.inputMethod.fullQualifiedClassName}`)")
    result
  }

  private def sanityCheck(solver: Z3Solver, asts: Iterable[(AST, String)], expect: Boolean, allowFail: Boolean, skip: Boolean): Unit = {
    val futures = Future.traverse(asts)({
      case (ast: AST, message: String) =>
        Future {
          sanityCheck(solver, ast, expect, allowFail, message, skip)
        }
    })
    Await.result(futures, Duration.Inf)
  }

  private def sanityCheck(solver: Z3Solver, ast: AST, expect: Boolean, allowFail: Boolean, message: String, skip: Boolean): Unit = {
    if (skip) return

    logger.info(message)
    solver.push()
    solver.mkAssert(ast)
    try {
      val result = solver.checkSAT(printUnsatCore = false)
      if (expect) assert(result)
      else assert(!result, solver.printModel())
    }
    catch {
      case e: Z3UnknownException =>
        logger.fatal(s"Sanity Check - Z3 returns UNKNOWN: ${e.message}")
      case e: AssertionError =>
        if (!allowFail) throw e
        else {
          logger.fatal(s"Sanity check failed!")
        }
      case e: Exception =>
        logger.fatal(s"Bound check - Unknown exception when running Z3: ${e.getMessage}")
    }
    solver.pop()
  }

  def ensureNoAssertion(methodTree: MethodTree): Unit = {
    collectCommands(methodTree.getBody).foreach({
      case assertTree: AssertTree => throw new Exception(s"Please do not use `assert` in the source code: `$assertTree`")
      case _ =>
    })
  }

  def extractBoundExpression(solver: Z3Solver, methodTree: MethodTree, typeContext: Map[String, BrboType]): AST = {
    val methodBody = methodTree.getBody
    if (methodBody == null)
      throw new Exception(s"There is no method body to extract bound expression from in $methodTree")
    val commands = collectCommands(methodTree.getBody)
    val assertions = commands.filter({
      case expressionStatementTree: ExpressionStatementTree =>
        expressionStatementTree.getExpression match {
          case methodInvocationTree: MethodInvocationTree =>
            val methodSelect = methodInvocationTree.getMethodSelect.toString
            if (methodSelect == "boundAssertion") true
            else false
          case _ => false
        }
      case _ => false
    })
    assert(assertions.size == 1, s"Please verify exactly 1 bound expression. Instead, we have `${assertions.size}` bound expression(s)")
    val boundExpression = assertions.head.asInstanceOf[ExpressionStatementTree].getExpression.asInstanceOf[MethodInvocationTree].getArguments.get(0)
    TreeUtils.translatePureExpressionToZ3AST(solver, boundExpression, typeContext)
  }

  private def generateDeltaVariableDoublePrime(identifier: String): String = {
    assert(GhostVariableUtils.isGhostVariable(identifier, Delta))
    s"$identifier\'\'"
  }

  def extractBoundAndCheck(decompositionResult: DecompositionResult, commandLineArguments: CommandLineArguments): Unit = {
    val inputMethod = decompositionResult.inputMethod
    val solver: Z3Solver = new Z3Solver
    BoundChecking.ensureNoAssertion(inputMethod.methodTree)
    val boundExpression: AST = BoundChecking.extractBoundExpression(solver, inputMethod.methodTree, inputMethod.inputVariables ++ inputMethod.localVariables)
    logger.info("")
    logger.info("")
    logger.info(s"Extracted bound expression is `$boundExpression`")
    checkBound(solver, decompositionResult, boundExpression, commandLineArguments)
  }

  case class GlobalInvariants(resourceInvariants: AST, deltaInvariants: AST, counterInvariants: AST, deltaCounterPairs: Set[DeltaCounterPair])

}
