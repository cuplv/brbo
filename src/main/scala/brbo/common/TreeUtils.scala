package brbo.common

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import com.microsoft.z3.AST
import com.sun.source.tree.Tree.Kind
import com.sun.source.tree._
import com.sun.source.util.TreePath
import javax.lang.model.`type`.TypeMirror
import org.apache.logging.log4j.LogManager

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

object TreeUtils {
  private val logger = LogManager.getLogger("brbo.common.TreeUtils")

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

  def collectCommands(statement: StatementTree): List[StatementTree] = {
    def throwException(message: String): Nothing = {
      throw new Exception(s"$message in AST: $statement")
    }

    if (statement == null) return Nil
    statement match {
      case tree@(_: AssertTree | _: BreakTree | _: ContinueTree | _: EmptyStatementTree |
                 _: ExpressionStatementTree | _: ReturnTree | _: VariableTree) =>
        tree :: Nil
      case tree: BlockTree => collectCommands(tree.getStatements.asScala)
      case _: ClassTree => throwException("Unexpected class tree")
      case _: DoWhileLoopTree => throwException("Not yet support do while loop tree") // collectCommands(tree.getStatement)
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

  def collectCommands(statements: Iterable[StatementTree]): List[StatementTree] = {
    statements.foldLeft(List[StatementTree]())({
      (acc, statement) => collectCommands(statement) ::: acc
    })
  }

  /**
   *
   * @param tree
   * @return All statement trees that are enclosed in `tree`
   */
  def collectStatementTrees(tree: StatementTree): Set[StatementTree] = {
    if (tree == null) return new HashSet[StatementTree]

    val set: Set[StatementTree] =
      tree match {
        case _@(_: AssertTree | _: BreakTree | _: ContinueTree | _: EmptyStatementTree |
                _: ExpressionStatementTree | _: ReturnTree | _: VariableTree) => new HashSet[StatementTree]()
        case tree2: BlockTree => collectStatementTrees(tree2.getStatements.asScala)
        case tree2: ForLoopTree =>
          collectStatementTrees(tree2.getInitializer.asScala) ++ collectStatementTrees(tree2.getStatement) ++ collectStatementTrees(tree2.getUpdate.asScala)
        case tree2: IfTree =>
          collectStatementTrees(tree2.getThenStatement) ++ collectStatementTrees(tree2.getElseStatement)
        case tree2: LabeledStatementTree => collectStatementTrees(tree2.getStatement)
        case tree2: WhileLoopTree => collectStatementTrees(tree2.getStatement)
        case _ => throw new Exception(s"Not yet support tree $tree (type: ${tree.getClass})")
      }
    set + tree
  }

  private def collectStatementTrees(trees: Iterable[StatementTree]): Set[StatementTree] = {
    trees.foldLeft(new HashSet[StatementTree])({ (acc, tree) => acc ++ collectStatementTrees(tree) })
  }

  def collectConditionTreesWithoutBrackets(tree: StatementTree): Set[ExpressionTree] = {
    if (tree == null) return new HashSet[ExpressionTree]

    val conditions = tree match {
      case _@(_: AssertTree | _: BreakTree | _: ContinueTree | _: EmptyStatementTree |
              _: ExpressionStatementTree | _: ReturnTree | _: VariableTree) => new HashSet[ExpressionTree]
      case tree2: BlockTree => collectConditionTreesWithoutBrackets(tree2.getStatements.asScala)
      case tree2: ForLoopTree =>
        collectConditionTreesWithoutBrackets(tree2.getInitializer.asScala) ++ collectConditionTreesWithoutBrackets(tree2.getStatement) ++
          collectConditionTreesWithoutBrackets(tree2.getUpdate.asScala) + tree2.getCondition
      case tree2: IfTree =>
        collectConditionTreesWithoutBrackets(tree2.getThenStatement) ++ collectConditionTreesWithoutBrackets(tree2.getElseStatement) + tree2.getCondition
      case tree2: LabeledStatementTree => collectConditionTreesWithoutBrackets(tree2.getStatement)
      case tree2: WhileLoopTree => collectConditionTreesWithoutBrackets(tree2.getStatement) + tree2.getCondition
      case _ => throw new Exception(s"Not yet support tree $tree (type: ${tree.getClass})")
    }
    conditions.map({
      condition => org.checkerframework.javacutil.TreeUtils.withoutParens(condition)
    })
  }

  private def collectConditionTreesWithoutBrackets(trees: Iterable[StatementTree]): Set[ExpressionTree] = {
    trees.foldLeft(new HashSet[ExpressionTree])({ (acc, tree) => acc ++ collectConditionTreesWithoutBrackets(tree) })
  }

  def modifiedVariables(tree: StatementTree): Set[String] = {
    collectCommands(tree).foldLeft(new HashSet[String])({
      (acc, command) =>
        command match {
          case expressionStatementTree: ExpressionStatementTree =>
            expressionStatementTree.getExpression match {
              case assignmentTree: AssignmentTree => acc + assignmentTree.getVariable.toString
              case unaryTree: UnaryTree =>
                unaryTree.getKind match {
                  case Tree.Kind.POSTFIX_INCREMENT | Tree.Kind.PREFIX_INCREMENT |
                       Tree.Kind.POSTFIX_DECREMENT | Tree.Kind.PREFIX_DECREMENT => acc + unaryTree.getExpression.toString
                  case _ => acc
                }
              case _ => acc
            }
          case variableTree: VariableTree =>
            if (variableTree.getInitializer != null) {
              logger.warn(s"Considering variable declarations as modifying variables: `$tree`")
              acc + variableTree.getName.toString
            }
            else acc
          case _ => acc
        }
    })
  }

  private val INDENT = 2

  @deprecated
  def treeToString(tree: StatementTree, indent: Int): String = {
    if (tree == null) return ""

    val indentString: String = " " * indent
    tree match {
      case _@(_: AssertTree | _: BreakTree | _: ContinueTree | _: EmptyStatementTree |
              _: ExpressionStatementTree | _: ReturnTree | _: VariableTree) => s"$indentString${tree.toString}"
      case tree2: BlockTree =>
        s"$indentString{\n${treeToString(tree2.getStatements.asScala, indent + INDENT)}\n$indentString}"
      case tree2: ForLoopTree =>
        val initializers = tree2.getInitializer.asScala
        val updates = tree2.getUpdate.asScala
        assert(initializers.size <= 1)
        assert(updates.size <= 1)
        s"${indentString}for (${treeToString(initializers.head, 0)}; ${tree2.getCondition}; ${treeToString(updates.head, 0)})\n${treeToString(tree2.getStatement, indent + INDENT)}"
      case tree2: IfTree =>
        s"${indentString}if ${tree2.getCondition}\n${treeToString(tree2.getThenStatement, indent + INDENT)}\n${indentString}else\n${treeToString(tree2.getElseStatement, indent + INDENT)}"
      case tree2: LabeledStatementTree => s"$indentString${tree2.getLabel}: ${treeToString(tree2.getStatement, indent)}"
      case tree2: WhileLoopTree =>
        s"${indentString}while ${tree2.getCondition} {\n${treeToString(tree2.getStatement, indent + INDENT)}\n}"
      case _ => throw new Exception(s"Not yet support tree $tree (type: ${tree.getClass})")
    }
  }

  @deprecated
  def treeToString(trees: Iterable[StatementTree], indent: Int): String = {
    trees.map(tree => treeToString(tree, indent)).mkString(";\n")
  }

  private val loopKinds = new java.util.HashSet[Tree.Kind]()
  loopKinds.add(Tree.Kind.FOR_LOOP)
  loopKinds.add(Tree.Kind.WHILE_LOOP)

  def getMinimalEnclosingLoop(path: TreePath): Option[Tree] = {
    org.checkerframework.javacutil.TreeUtils.enclosingOfKind(path, loopKinds) match {
      case null => None
      case loop => Some(loop)
    }
  }

  def getMaximalEnclosingLoop(path: TreePath): Option[Tree] = {
    var enclosingTrees: List[Tree] = Nil
    var p = path
    while (p != null) {
      val leaf = p.getLeaf
      assert(leaf != null) /*nninvariant*/
      enclosingTrees = leaf :: enclosingTrees
      p = p.getParentPath
    }
    enclosingTrees.find({
      enclosingTree => loopKinds.contains(enclosingTree.getKind)
    })
  }
}
