package brbo.common

import brbo.common.InvariantInference.logger
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import brbo.common.icra.{Assignment, Icra}
import brbo.common.instrument.FileFormat.C_FORMAT
import brbo.common.instrument.{InstrumentUtils, StatementTreeInstrumentation}
import brbo.common.instrument.InstrumentUtils.NewMethodInformation
import com.microsoft.z3.{AST, BoolExpr}
import org.apache.logging.log4j.LogManager

class InvariantInference(targetMethod: TargetMethod) {
  /**
   *
   * @param solver        The solver that is used to construct Z3 ASTs that represent the inferred invariants
   * @param locations     The locations before or after which we wish to infer invariants
   * @param freeVariables The inferred invariants must only contain free variables that appear in this set
   * @return The conjunction of local invariants that are existentially quantified by some variables
   */
  @deprecated
  def inferInvariant(solver: Z3Solver,
                     locations: Locations,
                     whichVariable: String,
                     freeVariables: Map[String, BrboType],
                     arguments: CommandLineArguments): BoolExpr = {
    logger.info(s"Infer invariants in method `${targetMethod.methodTree.getName}` `${locations.beforeOrAfter}` specified nodes in CFG")
    // If this assertion was too easy (e.g., `true` or `x>=0`), then it seems ICRA doesn't infer strong invariants!
    // Do not use `assert(x>0)`, because if the first assertion fails, then the invariants at the following assertion locations will be simply `false`
    // s"assert($whichVariable == $whichVariable)"
    // s"assert($whichVariable >= 101)"
    val cProgram = InvariantInference.translateToCAndInsertAssertions(targetMethod, locations, "true")
    // println(cProgram)
    Icra.runAndParseInvariant(cProgram, arguments.getIcraTimeout) match {
      case Some(parsedInvariants) =>
        val existentiallyQuantifiedInvariants = {
          parsedInvariants.map {
            parsedInvariant =>
              val (invariant, variableNames) = {
                val (invariant, variableNames) = Icra.translateToZ3AndCollectVariables(parsedInvariant.invariant, BOOL, solver)
                if (invariant == solver.mkFalse())
                  throw new Exception(s"ICRA infers invariant `$invariant`")
                else (invariant, variableNames)
              }
              // Intermediate variables must be existentially quantified
              var usedVariables: Set[String] = variableNames

              val equalities = {
                val equalities: List[AST] = parsedInvariant.declarations.map({
                  case Assignment(variable, expression) =>
                    val (variableAst, variableNames1) = Icra.translateToZ3AndCollectVariables(variable, INT, solver)
                    usedVariables = usedVariables ++ variableNames1
                    val (expressionAst, variableNames2) = Icra.translateToZ3AndCollectVariables(expression, INT, solver)
                    usedVariables = usedVariables ++ variableNames2
                    solver.mkEq(variableAst, expressionAst)
                  case x@_ => throw new Exception(s"Declaration $x should be parsed into `Assignment`")
                })
                solver.mkAnd(equalities: _*)
              }
              val constraint = solver.mkAnd(invariant, equalities)
              val existentiallyQuantify = usedVariables.filter(variable => !freeVariables.contains(variable)).map(variable => solver.mkIntVar(variable))
              if (existentiallyQuantify.isEmpty) {
                logger.debug(s"No local variable to quantify - Used variables: `$usedVariables`. Expected free variables: `$freeVariables`")
                constraint
              }
              else solver.mkExists(existentiallyQuantify, constraint)
          }
        }
        solver.mkOr(existentiallyQuantifiedInvariants: _*)
      case None =>
        logger.fatal("ICRA returns no invariant!")
        solver.mkTrue()
    }
  }
}

object InvariantInference {
  private val logger = LogManager.getLogger("brbo.common.InvariantInference")

  /**
   *
   * @param locations The locations before or after which we insert `assert(1)`
   * @return The C program that is translated from the input Java program, and is asserted with `assert(1)`
   */
  def translateToCAndInsertAssertions(targetMethod: TargetMethod, locations: Locations, assertion: String): String = {
    val indent = 2
    val ASSERT = s"assert($assertion)"

    val newMethodBody = InstrumentUtils.instrumentStatementTrees(
      targetMethod,
      StatementTreeInstrumentation(locations, _ => s"$ASSERT;"),
      indent
    )
    InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(
      targetMethod,
      NewMethodInformation(None, None, None, Nil, None, isAbstractClass = false, newMethodBody),
      C_FORMAT,
      indent
    )
  }
}