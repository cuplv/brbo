package brbo.common

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import com.microsoft.z3.AST
import com.sun.source.tree.Tree.Kind
import com.sun.source.tree._
import javax.lang.model.`type`.TypeMirror

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

object TreeUtils {
  def getAllInputVariables(methodTree: MethodTree): Map[String, BrboType] = {
    val parameters = methodTree.getParameters.asScala.foldLeft(HashMap[String, TypeMirror]())({
      (acc, param) => acc + (param.getName.toString -> org.checkerframework.javacutil.TreeUtils.typeOf(param.getType))
    })
    TypeUtils.typeMapTranslation(parameters)
  }

  def getAllDeclaredVariables(tree: StatementTree): Map[String, BrboType] = {
    collectCommands(tree).foldLeft(HashMap[String, BrboType]())({
      (acc, statement) =>
        statement match {
          case tree: VariableTree =>
            val name = tree.getName.toString
            val typ = org.checkerframework.javacutil.TreeUtils.typeOf(tree.getType)
            // Make sure that all variables in declared in a method have different names
            assert(!acc.keySet.contains(name), "Duplicate variable name: " + name)
            acc + (name -> TypeUtils.typeTranslation(typ))
          case _ => acc
        }
    })
  }

  def translatePureExpressionToZ3AST(solver: Z3Solver, expressionTree: ExpressionTree, typeContext: Map[String, BrboType]): AST = {
    def throwException(message: String): Nothing = {
      throw new Exception(s"Translate pure expressions to Z3 AST - $message in AST: $expressionTree")
    }

    expressionTree match {
      case _@(_: AnnotationTree | _: AssignmentTree | _: ArrayAccessTree | _: CompoundAssignmentTree |
              _: ErroneousTree | _: InstanceOfTree | _: MemberSelectTree | _: MethodInvocationTree |
              _: NewArrayTree | _: NewClassTree | _: TypeCastTree) =>
        throwException("Unsupported expression type")
      case tree: BinaryTree =>
        val left = translatePureExpressionToZ3AST(solver, tree.getLeftOperand, typeContext)
        val right = translatePureExpressionToZ3AST(solver, tree.getRightOperand, typeContext)

        tree.getKind match {
          case Kind.PLUS => solver.mkAdd(left, right)
          case Kind.MINUS => solver.mkSub(left, right)
          case Kind.MULTIPLY => solver.mkMul(left, right)
          case Kind.LESS_THAN => solver.mkLt(left, right)
          case Kind.LESS_THAN_EQUAL => solver.mkLe(left, right)
          case Kind.GREATER_THAN => solver.mkGt(left, right)
          case Kind.GREATER_THAN_EQUAL => solver.mkGe(left, right)
          case Kind.CONDITIONAL_AND => solver.mkAnd(left, right)
          case Kind.CONDITIONAL_OR => solver.mkOr(left, right)
          case Kind.EQUAL_TO => solver.mkEq(left, right)
          case Kind.NOT_EQUAL_TO => solver.mkNe(left, right)
          case _ => throwException("Unsupported binary tree")
        }
      case tree: ConditionalExpressionTree =>
        solver.mkITE(
          translatePureExpressionToZ3AST(solver, tree.getCondition, typeContext),
          translatePureExpressionToZ3AST(solver, tree.getTrueExpression, typeContext),
          translatePureExpressionToZ3AST(solver, tree.getFalseExpression, typeContext)
        )
      case tree: IdentifierTree =>
        val identifier = tree.getName.toString
        typeContext.get(identifier) match {
          case Some(typ) =>
            typ match {
              case INT => solver.mkIntVar(identifier)
              case BOOL => solver.mkBoolVar(identifier)
            }
          case None => throwException(s"Type context does not have type information for variable $identifier")
        }
      case tree: LiteralTree =>
        tree.getKind match {
          case Kind.INT_LITERAL => solver.mkIntVal(tree.getValue.asInstanceOf[Int])
          case Kind.BOOLEAN_LITERAL => solver.mkBoolVal(tree.getValue.asInstanceOf[Boolean])
          case _ => throwException("Unsupported literal")
        }
      case tree: ParenthesizedTree => translatePureExpressionToZ3AST(solver, tree.getExpression, typeContext)
      case tree: UnaryTree =>
        tree.getKind match {
          case Kind.UNARY_MINUS => solver.mkSub(solver.mkIntVal(0), translatePureExpressionToZ3AST(solver, tree.getExpression, typeContext))
          case Kind.UNARY_PLUS => translatePureExpressionToZ3AST(solver, tree.getExpression, typeContext)
          case _ => throwException("Unsupported expression type")
        }
    }
  }

  private def collectCommands(statements: Iterable[StatementTree]): List[StatementTree] = {
    statements.foldLeft(List[StatementTree]())({
      (acc, statement) => collectCommands(statement) ::: acc
    })
  }

  def collectCommands(statement: StatementTree): List[StatementTree] = {
    def throwException(message: String): Nothing = {
      throw new Exception(s"Collect commands - $message in AST: $statement")
    }

    if (statement == null) return Nil
    statement match {
      case tree@(_: AssertTree | _: BreakTree | _: ContinueTree | _: EmptyStatementTree |
                 _: ExpressionStatementTree | _: ReturnTree | _: VariableTree) =>
        tree :: Nil
      case tree: BlockTree => collectCommands(tree.getStatements.asScala)
      case _: ClassTree => throwException("Unexpected class tree")
      case tree: DoWhileLoopTree => collectCommands(tree.getStatement)
      case _: EnhancedForLoopTree => throwException("Not yet support enhanced for tree")
      case tree: ForLoopTree =>
        val statements = tree.getInitializer.asScala.toList ::: tree.getStatement :: tree.getUpdate.asScala.toList
        collectCommands(statements)
      case tree: IfTree => collectCommands(tree.getThenStatement) ::: collectCommands(tree.getElseStatement) ::: Nil
      case tree: LabeledStatementTree => collectCommands(tree.getStatement)
      case _: SwitchTree => throwException("Not yet support switch tree")
      case _: SynchronizedTree => throwException("Not yet support synchronized tree")
      case _: ThrowTree => throwException("Not yet support throw tree")
      case _: TryTree => throwException("Not yet support try tree")
      case tree: WhileLoopTree => collectCommands(tree.getStatement)
    }
  }
}
