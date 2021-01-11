package brbo.common

import brbo.common.InstrumentUtils.AtomicStatementInstrumentation
import brbo.common.InstrumentUtils.FileFormat.{JAVA_FORMAT, C_FORMAT}
import brbo.common.InstrumentUtils.InstrumentMode.ALL
import brbo.common.InvariantInference.BeforeOrAfter.{AFTER, BEFORE, BeforeOrAfter}
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import brbo.common.icra.{Assignment, Icra}
import com.microsoft.z3.{AST, Expr}
import com.sun.source.tree.{MethodTree, Tree}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.node.Node

object InvariantInference {
  private val logger = LogManager.getLogger("brbo.common.InvariantInference")

  /**
   *
   * @param solver                The solver that is used to construct Z3 ASTs that represent the inferred invariants
   * @param className             The class name of the method where we wish to infer invariants
   * @param methodTree            The method where we wish to infer invariants
   * @param getLineNumber         A function to get line numbers
   * @param cfg                   The control flow graph of the method where we wish to infer invariants
   * @param locations             The locations before or after which we wish to infer invariants
   * @param existentiallyQuantify The inferred invariants must existentially quantify these variables
   * @param freeVariables         The inferred invariants must only contain free variables that appear in this set
   * @return The conjunction of local invariants that are existentially quantified by some variables
   */
  def inferInvariant(solver: Z3Solver,
                     className: String,
                     methodTree: MethodTree,
                     getLineNumber: Tree => Int,
                     cfg: ControlFlowGraph,
                     locations: Locations,
                     existentiallyQuantify: Map[String, BrboType],
                     freeVariables: Map[String, BrboType]): AST = {
    // Intermediate variables must be existentially quantified
    def getExtraExistentiallyQuantify(variables: Set[String]): Set[String] = {
      variables.filter(variable => !existentiallyQuantify.contains(variable) && !freeVariables.contains(variable))
    }

    logger.info(s"Infer invariants in method ${methodTree.getName} ${locations.beforeOrAfter} specified nodes in CFG")
    val cProgram = translateToCAndInsertAssertions(className, methodTree, getLineNumber, cfg, locations)
    Icra.run(cProgram) match {
      case Some(parsedInvariants) =>
        val existentiallyQuantifiedInvariants =
          parsedInvariants.map {
            parsedInvariant =>
              val (invariant, variableNames) = Icra.translateToZ3AndCollectVariables(parsedInvariant.invariant, BOOL, solver)
              var extraExistentiallyQuantify = getExtraExistentiallyQuantify(variableNames)

              val equalities = {
                val equalities: List[AST] = parsedInvariant.declarations.map({
                  case Assignment(variable, expression) =>
                    val (variableAst, variableNames1) = Icra.translateToZ3AndCollectVariables(variable, INT, solver)
                    extraExistentiallyQuantify = extraExistentiallyQuantify ++ getExtraExistentiallyQuantify(variableNames1)
                    val (expressionAst, variableNames2) = Icra.translateToZ3AndCollectVariables(expression, INT, solver)
                    extraExistentiallyQuantify = extraExistentiallyQuantify ++ getExtraExistentiallyQuantify(variableNames2)
                    solver.mkEq(variableAst, expressionAst)
                  case x@_ => throw new Exception(s"Declaration $x should be parsed into `Assignment`")
                })
                solver.mkAnd(equalities: _*)
              }
              solver.mkExists(
                existentiallyQuantify.map({
                  case (identifier, typ) =>
                    typ match {
                      case INT => solver.mkIntVar(identifier)
                      case BOOL => solver.mkBoolVar(identifier)
                    }
                }) ++ extraExistentiallyQuantify.map(variable => solver.mkIntVar(variable)),
                solver.mkAnd(invariant, equalities).asInstanceOf[Expr]
              )
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
  private def translateToCAndInsertAssertions(className: String,
                                              methodTree: MethodTree,
                                              getLineNumber: Tree => Int,
                                              cfg: ControlFlowGraph,
                                              locations: Locations): String = {
    val indent = 2
    val ASSERT_TRUE = "assert(true)"

    val methodBody = methodTree.getBody

    val result = InstrumentUtils.substituteAtomicStatements(
      methodBody,
      AtomicStatementInstrumentation(
        {
          node: Node => locations.whichASTs.apply(node)
        },
        {
          tree: Tree =>
            locations.beforeOrAfter match {
              case BEFORE => s"$ASSERT_TRUE; ${tree.toString};"
              case AFTER => s"${tree.toString}; $ASSERT_TRUE;"
            }
        }
      ),
      indent,
      cfg,
      getLineNumber,
      ALL
    )
    val newMethodBody = result.result
    InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(
      methodTree,
      className,
      newMethodBody,
      C_FORMAT,
      indent
    )
  }

  /**
   *
   * @param whichASTs     Insert `assert(1)` before / after Which ASTs. We use `Node` instead of `Tree`
   *                      because we are using `Node` as intermediate representations that are less
   *                      "syntactic" than ASTs
   * @param beforeOrAfter Insert `assert(1)` either before or after the ASTs that satisfy the condition
   */
  case class Locations(whichASTs: Node => Boolean, beforeOrAfter: BeforeOrAfter)

  object BeforeOrAfter extends Enumeration {
    type BeforeOrAfter = Value
    val BEFORE, AFTER = Value
  }

}
