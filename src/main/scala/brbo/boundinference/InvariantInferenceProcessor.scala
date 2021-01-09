package brbo.boundinference

import brbo.boundinference.BeforeOrAfter.{AFTER, BEFORE, BeforeOrAfter}
import brbo.common.FileFormat.C_FORMAT
import brbo.common.InstrumentUtils.AtomicStatementInstrumentation
import brbo.common.InstrumentUtils.InstrumentMode.ALL
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import brbo.common.icra.{Assignment, Icra}
import brbo.common.{InstrumentUtils, Z3Solver}
import com.microsoft.z3.{AST, Expr}
import com.sun.source.tree.Tree
import javax.annotation.processing.SupportedAnnotationTypes
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.node.Node

@SupportedAnnotationTypes(Array("*"))
class InvariantInferenceProcessor(solver: Z3Solver) extends BasicProcessor {
  private val logger = LogManager.getLogger(classOf[InvariantInferenceProcessor])

  def inferInvariant(locations: Locations, existentiallyQuantify: Map[String, BrboType]): AST = {
    val cProgram = translateToCAndInsertAssertions(locations)
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
        solver.mkBoolVal(true)
    }
  }

  /**
   *
   * @param locations The locations before or after which we insert `assert(1)`
   * @return The C program that is translated from the input Java program, and is asserted with `assert(1)`
   */
  def translateToCAndInsertAssertions(locations: Locations): String = {
    assumeOneClassOneMethod()

    val ASSERT_TRUE = "assert(true);"

    getMethods.head match {
      case (methodTree, cfg) =>
        logger.debug(s"Translate to C programs and insert assertions ")
        val result = InstrumentUtils.substituteAtomicStatements(
          methodTree.getBody,
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
        val methodBody = result.result
        InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(
          methodTree,
          getEnclosingClass(methodTree).get.getSimpleName.toString,
          methodBody,
          C_FORMAT,
          indent
        )
    }
  }

}

case class Locations(whichASTs: Node => Boolean, beforeOrAfter: BeforeOrAfter)

object BeforeOrAfter extends Enumeration {
  type BeforeOrAfter = Value
  val BEFORE, AFTER = Value
}
