package brbo.common

import brbo.common.FileFormat.C_FORMAT
import brbo.common.InstrumentUtils.AtomicStatementInstrumentation
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
   * @param className             The class name
   * @param methodTree            The method where we wish to infer invariants
   * @param getLineNumber         A function to get line numbers
   * @param cfg                   The control flow graph
   * @param locations             The locations before or after which we wish to infer invariants
   * @param existentiallyQuantify The inferred invariants will be existentially quantified by these variables
   * @return The conjunction of local invariants that are existentially quantified by some variables
   */
  def inferInvariant(solver: Z3Solver,
                     className: String,
                     methodTree: MethodTree,
                     getLineNumber: Tree => Int,
                     cfg: ControlFlowGraph,
                     locations: Locations,
                     existentiallyQuantify: Map[String, BrboType]): AST = {
    logger.debug(s"Infer invariants")
    val cProgram = translateToCAndInsertAssertions(className, methodTree, getLineNumber, cfg, locations)
    Icra.run(cProgram) match {
      case Some(parsedInvariants) =>
        val existentiallyQuantifiedInvariants =
          parsedInvariants.map {
            parsedInvariant =>
              val invariant: AST = Icra.translateToZ3(parsedInvariant.invariant, BOOL, solver)
              val equalities = {
                val equalities: List[AST] = parsedInvariant.declarations.map({
                  case Assignment(variable, expression) =>
                    val variableAst = Icra.translateToZ3(variable, INT, solver)
                    val expressionAst = Icra.translateToZ3(expression, INT, solver)
                    solver.mkEq(variableAst, expressionAst)
                  case x@_ => throw new Exception(s"Declaration $x should be parsed into `Assignment`")
                })
                solver.mkAnd(equalities: _*)
              }
              solver.mkExists(
                existentiallyQuantify.map({
                  case (identifier, typ) =>
                    val variable =
                      typ match {
                        case BOOL => solver.mkBoolVar(identifier)
                        case INT => solver.mkIntVar(identifier)
                      }
                    variable.asInstanceOf[Expr]
                }),
                solver.mkAnd(invariant, equalities).asInstanceOf[Expr]
              )
          }
        solver.mkAnd(existentiallyQuantifiedInvariants: _*)
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
    val ASSERT_TRUE = "assert(true);"

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
              case BEFORE => s"$ASSERT_TRUE ${tree.toString};"
              case AFTER => s"${tree.toString}; $ASSERT_TRUE"
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
