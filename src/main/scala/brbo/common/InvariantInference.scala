package brbo.common

import brbo.common.InstrumentUtils.FileFormat.C_FORMAT
import brbo.common.InstrumentUtils.{NewMethodInformation, StatementTreeInstrumentation}
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import brbo.common.icra.{Assignment, Icra}
import com.microsoft.z3.{AST, BoolExpr, Expr}
import org.apache.logging.log4j.LogManager

class InvariantInference(targetMethod: TargetMethod) {
  private val logger = LogManager.getLogger(classOf[InvariantInference])

  private val methodTree = targetMethod.methodTree

  /**
   *
   * @param solver                The solver that is used to construct Z3 ASTs that represent the inferred invariants
   * @param locations             The locations before or after which we wish to infer invariants
   * @param freeVariables         The inferred invariants must only contain free variables that appear in this set
   * @return The conjunction of local invariants that are existentially quantified by some variables
   */
  def inferInvariant(solver: Z3Solver,
                     locations: Locations,
                     whichVariable: String,
                     freeVariables: Map[String, BrboType]): BoolExpr = {
    logger.info(s"Infer invariants in method `${methodTree.getName}` `${locations.beforeOrAfter}` specified nodes in CFG")
    val cProgram = translateToCAndInsertAssertions(locations, whichVariable)
    Icra.run(cProgram) match {
      case Some(parsedInvariants) =>
        val existentiallyQuantifiedInvariants =
          parsedInvariants.map {
            parsedInvariant =>
              val (invariant, variableNames) = Icra.translateToZ3AndCollectVariables(parsedInvariant.invariant, BOOL, solver)
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
              val constraint = solver.mkAnd(invariant, equalities).asInstanceOf[Expr]
              val existentiallyQuantify = usedVariables.filter(variable => !freeVariables.contains(variable)).map(variable => solver.mkIntVar(variable))
              if (existentiallyQuantify.isEmpty) {
                logger.debug(s"No local variable to quantify - Used variables: `$usedVariables`. Expected free variables: `$freeVariables`")
                constraint
              }
              else solver.mkExists(existentiallyQuantify, constraint)
          }
        solver.mkOr(existentiallyQuantifiedInvariants: _*)
      case None =>
        logger.fatal("ICRA returns no invariant!")
        solver.mkTrue()
    }
  }

  /**
   *
   * @param locations The locations before or after which we insert `assert(1)`
   * @return The C program that is translated from the input Java program, and is asserted with `assert(1)`
   */
  private def translateToCAndInsertAssertions(locations: Locations, whichVariable: String): String = {
    val indent = 2
    val ASSERT_TRUE = s"assert($whichVariable == $whichVariable)"

    val newMethodBody = InstrumentUtils.instrumentStatementTrees(
      targetMethod,
      StatementTreeInstrumentation(locations, _ => s"$ASSERT_TRUE;"),
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
