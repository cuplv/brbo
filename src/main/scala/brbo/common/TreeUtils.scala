package brbo.common

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import brbo.verification.BoundChecking
import com.microsoft.z3.AST
import com.sun.source.tree.Tree.Kind
import com.sun.source.tree._
import com.sun.source.util.TreePath
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.node.Node

import java.util
import javax.lang.model.`type`.TypeMirror
import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

object TreeUtils {
  private val logger = LogManager.getLogger("brbo.common.TreeUtils")

  def acceptableTree(tree: StatementTree): Unit = {
    if (tree == null) return

    tree match {
      case _ if isCommand(tree) =>
        tree match {
          case variableTree: VariableTree => assert(variableTree.getInitializer != null, s"Variable declaration must have initializers: `$tree`")
          case _ =>
        }
      case blockTree: BlockTree => blockTree.getStatements.asScala.foreach(t => acceptableTree(t))
      case forLoopTree: ForLoopTree =>
        forLoopTree.getInitializer.asScala.foreach(t => acceptableTree(t))
        mustBeBlockTreeOrNull(forLoopTree.getStatement, forLoopTree)
        acceptableTree(forLoopTree.getStatement)
        forLoopTree.getUpdate.asScala.foreach(t => acceptableTree(t))
      case ifTree: IfTree =>
        mustBeBlockTreeOrNull(ifTree.getThenStatement, ifTree)
        mustBeBlockTreeOrNull(ifTree.getElseStatement, ifTree)
        acceptableTree(ifTree.getThenStatement)
        acceptableTree(ifTree.getElseStatement)
      // case labeledStatementTree: LabeledStatementTree => acceptableTree(labeledStatementTree.getStatement)
      case whileLoopTree: WhileLoopTree =>
        mustBeBlockTreeOrNull(whileLoopTree.getStatement, whileLoopTree)
        acceptableTree(whileLoopTree.getStatement)
      case _ => throw new Exception(s"Unsupported tree: `$tree`")
    }
  }

  // Ensure inserting statements still preserves the original control flow
  private def mustBeBlockTreeOrNull(tree: Tree, enclosingTree: Tree): Unit = {
    tree match {
      case null =>
      case _: BlockTree =>
      case _ => throw new Exception(s"Tree `$tree` must be a block tree in `$enclosingTree`")
    }
  }

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
          case None =>
            if (identifier == BoundChecking.MAX_COEFFICIENT)
              solver.mkIntVal(BoundChecking.MAX_COEFFICIENT_VALUE)
            else
              throwException(s"Type context does not have type information for variable $identifier")
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
          case Kind.LOGICAL_COMPLEMENT => solver.mkNot(translatePureExpressionToZ3AST(solver, tree.getExpression, typeContext))
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
      case _ if isCommand(statement) => statement :: Nil
      case tree: BlockTree => collectCommands(tree.getStatements.asScala)
      case _: ClassTree => throwException("Unexpected class tree")
      case _: DoWhileLoopTree => throwException("Not yet support do while loop tree") // collectCommands(tree.getStatement)
      case _: EnhancedForLoopTree => throwException("Not yet support enhanced for tree")
      case tree: ForLoopTree =>
        val statements = tree.getInitializer.asScala.toList ::: tree.getStatement :: tree.getUpdate.asScala.toList
        collectCommands(statements)
      case tree: IfTree => collectCommands(tree.getThenStatement) ::: collectCommands(tree.getElseStatement) ::: Nil
      // case tree: LabeledStatementTree => collectCommands(tree.getStatement)
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
   * @return All statement trees that are enclosed in `tree`
   */
  def collectStatementTrees(tree: StatementTree): Set[StatementTree] = {
    if (tree == null) return new HashSet[StatementTree]

    val set: Set[StatementTree] =
      tree match {
        case _ if isCommand(tree) => new HashSet[StatementTree]()
        case tree2: BlockTree => collectStatementTrees(tree2.getStatements.asScala)
        case tree2: ForLoopTree =>
          collectStatementTrees(tree2.getInitializer.asScala) ++ collectStatementTrees(tree2.getStatement) ++ collectStatementTrees(tree2.getUpdate.asScala)
        case tree2: IfTree =>
          collectStatementTrees(tree2.getThenStatement) ++ collectStatementTrees(tree2.getElseStatement)
        // case tree2: LabeledStatementTree => collectStatementTrees(tree2.getStatement)
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
      case _ if isCommand(tree) => new HashSet[ExpressionTree]
      case tree2: BlockTree => collectConditionTreesWithoutBrackets(tree2.getStatements.asScala)
      case tree2: ForLoopTree =>
        collectConditionTreesWithoutBrackets(tree2.getInitializer.asScala) ++ collectConditionTreesWithoutBrackets(tree2.getStatement) ++
          collectConditionTreesWithoutBrackets(tree2.getUpdate.asScala) + tree2.getCondition
      case tree2: IfTree =>
        collectConditionTreesWithoutBrackets(tree2.getThenStatement) ++ collectConditionTreesWithoutBrackets(tree2.getElseStatement) + tree2.getCondition
      // case tree2: LabeledStatementTree => collectConditionTreesWithoutBrackets(tree2.getStatement)
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

  def isCommand(tree: StatementTree): Boolean = {
    if (tree == null) return false

    tree match {
      case _@(_: AssertTree | _: EmptyStatementTree | _: BreakTree | _: ContinueTree |
              _: ExpressionStatementTree | _: ReturnTree | _: VariableTree) => true
      case _ => false
    }
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
              logger.trace(s"Consider variable declarations as modifying variables: `$tree`")
              acc + variableTree.getName.toString
            }
            else acc
          case _ => acc
        }
    })
  }

  private val INDENT = 2

  val loopKinds: Set[Tree.Kind] = HashSet[Tree.Kind](Tree.Kind.FOR_LOOP, Tree.Kind.WHILE_LOOP)

  def getMinimalEnclosingLoop(path: TreePath): Option[StatementTree] = {
    org.checkerframework.javacutil.TreePathUtil.enclosingOfKind(path, loopKinds.asJava) match {
      case null => None
      case loop => Some(loop.asInstanceOf[StatementTree])
    }
  }

  def getMaximalEnclosingLoop(path: TreePath): Option[Tree] = {
    val enclosingTrees: List[Tree] = getEnclosingTrees(path)
    enclosingTrees.find({
      enclosingTree => loopKinds.contains(enclosingTree.getKind)
    })
  }

  def getEnclosingTrees(path: TreePath): List[Tree] = {
    var enclosingTrees: List[Tree] = Nil
    var p = path
    while (p != null) {
      val leaf = p.getLeaf
      assert(leaf != null)
      enclosingTrees = leaf :: enclosingTrees
      p = p.getParentPath
    }
    assert(enclosingTrees.head.isInstanceOf[CompilationUnitTree])
    assert(enclosingTrees.tail.head.isInstanceOf[ClassTree])
    assert(enclosingTrees.tail.tail.head.isInstanceOf[MethodTree])
    enclosingTrees
  }

  def getEnclosingStatementTrees(path: TreePath): List[StatementTree] = {
    getEnclosingTrees(path).tail.tail.tail.map(t => t.asInstanceOf[StatementTree])
  }

  def getNodesCorrespondingToCommand(controlFlowGraph: ControlFlowGraph, tree: StatementTree): Set[Node] = {
    assert(isCommand(tree))
    val nodes = tree match {
      case _@(_: AssertTree | _: EmptyStatementTree | _: ReturnTree | _: VariableTree | _: ContinueTree | _: BreakTree) =>
        val set = controlFlowGraph.getNodesCorrespondingToTree(tree)
        if (set == null) new util.HashSet[Node]()
        else set
      case command: ExpressionStatementTree => controlFlowGraph.getNodesCorrespondingToTree(command.getExpression)
    }
    nodes.asScala.toSet
  }
}
