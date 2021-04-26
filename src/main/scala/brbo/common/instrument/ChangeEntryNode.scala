package brbo.common.instrument

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.TypeUtils.BrboType
import brbo.common.instrument.FileFormat.JAVA_FORMAT
import brbo.common.instrument.InstrumentUtils.{NewMethodInformation, appendSemiColon}
import brbo.common.instrument.JumpOrNormal.{Jump, JumpOrNormal, Normal}
import brbo.common.{GhostVariableUtils, TargetMethod, TreeUtils}
import brbo.verification.BasicProcessor
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager

import scala.collection.JavaConverters._

object ChangeEntryNode {
  private val logger = LogManager.getLogger("brbo.common.instrument.ChangeEntryNode")

  /**
   *
   * @param inputMethod      A method to be instrumented
   * @param entryTree        A command in the input method, which will be the entry command of the new method
   * @param preservedUpdates Resource updates that are preserved. All other updates are discarded. The goal is
   *                         to establish the relation between resource updates of a group in the original
   *                         program, and resource updates in the new program
   * @return A method that is almost the same as the input method, but with a different entry command
   */
  def changeEntryNode(inputMethod: TargetMethod, entryTree: StatementTree, preservedUpdates: Set[StatementTree], testMode: Boolean): TargetMethod = {
    assert(TreeUtils.isCommand(entryTree))
    val entryTreeAst = treeToAst(entryTree)

    /**
     *
     * @return ALl paths (of the original program) that begin and end with the current tree,
     *         along with the "direction" of the immediate next control flow
     */
    def handleBreakContinue(currentTree: StatementTree, enclosingLoop: Option[StatementTree]): FatNode = {
      if (currentTree == null)
        return EmptyNode
      logger.trace(s"Break: `$currentTree`")
      currentTree match {
        case tree if TreeUtils.isCommand(currentTree) =>
          tree match {
            case _: BreakTree =>
              enclosingLoop match {
                case Some(loop) =>
                  loop match {
                    case _@(_: WhileLoopTree | _: ForLoopTree) => LeafNode(Data(Empty, Jump))
                    case _ => throw new Exception("Unexpected")
                  }
                case None => throw new Exception("Unexpected")
              }
            case _: ContinueTree =>
              enclosingLoop match {
                case Some(loop) =>
                  loop match {
                    case _: WhileLoopTree => LeafNode(Data(Empty, Jump))
                    case forLoopTree: ForLoopTree =>
                      val updates = Block(forLoopTree.getUpdate.asScala.map(t => treeToAst(t)).toList)
                      LeafNode(Data(updates, Jump))
                    case _ => throw new Exception("Unexpected")
                  }
                case None => throw new Exception("Unexpected")
              }
            case _ => LeafNode(Data(treeToAst(tree), Normal))
          }
        case tree: BlockTree =>
          var node: FatNode = EmptyNode
          val statements = tree.getStatements.asScala
          var i = 0
          while (i < statements.size) {
            val toAppend = handleBreakContinue(statements(i), enclosingLoop)
            node = FatNode.append(node, toAppend)
            i = i + 1
          }
          node
        case tree: IfTree =>
          BranchNode(tree.getCondition,
            handleBreakContinue(tree.getThenStatement, enclosingLoop), handleBreakContinue(tree.getElseStatement, enclosingLoop))
        case tree: WhileLoopTree => LeafNode(Data(treeToAst(tree), Normal))
        case tree: ForLoopTree => LeafNode(Data(treeToAst(tree), Normal))
        case _ => logger.fatal(s"$currentTree (${currentTree.getClass})"); throw new Exception("Unexpected")
      }
    }

    /**
     *
     * @return All paths of the original program (expressed with an AST) that begin with the immediate next control location of the current tree
     */
    def handleNextPaths(currentTree: StatementTree): AST = {
      logger.trace(s"Next: `$currentTree`")
      val enclosingTrees: List[StatementTree] = {
        val enclosingTrees = TreeUtils.getEnclosingStatementTrees(inputMethod.getPath(currentTree))
        assert(enclosingTrees.last == currentTree)
        enclosingTrees
      }
      if (enclosingTrees.size == 1) return Empty

      logger.trace(s"Enclosing: ${enclosingTrees(enclosingTrees.size - 2)}")
      val enclosingTree = enclosingTrees(enclosingTrees.size - 2)
      enclosingTree match {
        case tree: BlockTree =>
          val enclosingLoop = TreeUtils.getMinimalEnclosingLoop(inputMethod.getPath(tree))
          var foundEntryTree = false
          var node: FatNode = EmptyNode
          val statements = tree.getStatements.asScala
          var i = 0
          while (i < statements.size) {
            if (statements(i) == currentTree) {
              foundEntryTree = true
            }
            else {
              if (foundEntryTree) {
                val toAppend = handleBreakContinue(statements(i), enclosingLoop)
                node = FatNode.append(node, toAppend)
              }
            }
            i = i + 1
          }

          val normalNextPath = handleNextPaths(tree)
          val jumpNextPath = enclosingLoop match {
            case Some(loop) =>
              Block(List(loopBodySubstitute(loop, entryTreeAst), handleNextPaths(loop)))
            case None => Empty
          }
          FatNode.appendAst(node, normalNextPath, jumpNextPath)
        case tree: IfTree => handleNextPaths(tree)
        case tree@(_: WhileLoopTree | _: ForLoopTree) =>
          Block(List(loopBodySubstitute(tree, entryTreeAst), handleNextPaths(tree)))
        case _ => logger.fatal(s"$enclosingTree (${enclosingTree.getClass})"); throw new Exception("Unexpected")
      }
    }

    val newAst = handleNextPaths(entryTree)
    val parameters = (inputMethod.inputVariables ++ inputMethod.localVariables).toList.map(pair => BrboType.variableDeclaration(pair._1, pair._2)).sorted.mkString(", ")
    val newMethodBody = s"{\n${newAst.print(preserveDeclaration = false, preservedUpdates)}\n}"
    val methodInformation = {
      if (testMode) NewMethodInformation(Some(parameters), None, None, Nil, None, isAbstractClass = false, newMethodBody)
      else NewMethodInformation(Some(parameters), None, None, List("import brbo.benchmarks.Common;"), Some("Common"), isAbstractClass = true, newMethodBody)
    }
    val newSourceCode = InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(inputMethod, methodInformation, JAVA_FORMAT, indent = 2)
    BasicProcessor.getTargetMethod(inputMethod.fullQualifiedClassName, newSourceCode)
  }

  def treeToAst(tree: StatementTree): AST = {
    if (tree == null)
      return Empty
    tree match {
      case tree if TreeUtils.isCommand(tree) => Command(tree)
      case tree: BlockTree => Block(tree.getStatements.asScala.toList.map(s => treeToAst(s)))
      case tree: IfTree => ITE(tree.getCondition, treeToAst(tree.getThenStatement), treeToAst(tree.getElseStatement))
      case tree: ForLoopTree =>
        val initializer = tree.getInitializer.asScala.toList.map(s => treeToAst(s))
        val body = tree.getStatement :: tree.getUpdate.asScala.toList
        Block(initializer :+ Loop(tree.getCondition, Block(body.map(s => treeToAst(s)))))
      case tree: WhileLoopTree => Loop(tree.getCondition, treeToAst(tree.getStatement))
      case _ => throw new Exception(s"Unexpected tree: $tree (${tree.getClass})")
    }
  }

  /**
   *
   * @return Substitute ALL occurrences of oldAST with newAST in ast
   */
  def substitute(ast: AST, oldAst: AST, newAst: AST): AST = {
    if (ast == oldAst) newAst
    else {
      ast match {
        case block: Block => Block(block.statements.map(s => substitute(s, oldAst, newAst)))
        case loop: Loop => Loop(loop.condition, substitute(loop.body, oldAst, newAst))
        case ite: ITE => ITE(ite.condition, substitute(ite.thenAst, oldAst, newAst), substitute(ite.elseAst, oldAst, newAst))
        case _: Command => ast
        case Return => Return
        case Empty => Empty
      }
    }
  }

  private def loopBodySubstitute(loop: StatementTree, toBeSubstituted: AST): AST = {
    val loopBody = loop match {
      case forLoopTree: ForLoopTree =>
        Loop(forLoopTree.getCondition, Block(treeToAst(forLoopTree.getStatement) :: forLoopTree.getUpdate.asScala.toList.map(t => treeToAst(t))))
      case whileLoopTree: WhileLoopTree => treeToAst(whileLoopTree)
      case _ => throw new Exception("Unexpected")
    }
    substitute(loopBody, toBeSubstituted, Return)
  }
}

sealed trait AST {
  /**
   *
   * @param preserveDeclaration If x is a local variable of the original program, then ensure x is never declared again
   *                            in the body of the new program, by turning on this flag, i.e., not declaring variables
   *                            in the body of the new program
   * @param preservedUpdates    Preserve these resource updates and discard all others
   * @return
   */
  def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String
}

case class Block(statements: List[AST]) extends AST {
  override def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String = {
    s"{ ${statements.map(t => t.print(preserveDeclaration, preservedUpdates)).mkString(" ")} }"
  }
}

case class Loop(condition: ExpressionTree, body: AST) extends AST {
  override def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String = {
    val conditionString = {
      val string = condition.toString
      if (string.startsWith("(") && string.endsWith(")")) string
      else s"($string)"
    }
    s"while $conditionString ${body.print(preserveDeclaration, preservedUpdates)}"
  }
}

case class ITE(condition: ExpressionTree, thenAst: AST, elseAst: AST) extends AST {
  override def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String = {
    val conditionString = {
      val string = condition.toString
      if (string.startsWith("(") && string.endsWith(")")) string
      else s"($string)"
    }
    s"if $conditionString ${thenAst.print(preserveDeclaration, preservedUpdates)} else ${elseAst.print(preserveDeclaration, preservedUpdates)}"
  }
}

case class Command(statement: StatementTree) extends AST {
  assert(TreeUtils.isCommand(statement))

  override def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String = {
    val string = {
      statement match {
        case variableTree: VariableTree =>
          if (preserveDeclaration) statement.toString
          else s"${variableTree.getName.toString} = ${variableTree.getInitializer}"
        case _ =>
          statement match {
            case expressionStatementTree: ExpressionStatementTree =>
              if (GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource).isDefined) {
                if (preservedUpdates.contains(statement)) statement.toString
                else ";"
              }
              else statement.toString
            case _ => statement.toString
          }
      }
    }
    appendSemiColon(string)
  }
}

case object Return extends AST {
  // To avoid "unreachable statement" error from javac, we can use `if (true) return;`
  override def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String = "if (true) return;"
}

case object Empty extends AST {
  override def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String = ";"
}

object JumpOrNormal extends Enumeration {
  type JumpOrNormal = Value
  val Jump, Normal = Value
}

case class Data(ast: AST, breakOrContinue: JumpOrNormal)

object FatNode {
  def append(fatNode: FatNode, newNode: FatNode): FatNode = {
    fatNode match {
      case BranchNode(condition, thenNode, elseNode) =>
        BranchNode(condition, append(thenNode, newNode), append(elseNode, newNode))
      case SequenceNode(next, data) =>
        SequenceNode(append(next, newNode), data)
      case LeafNode(data) =>
        data.breakOrContinue match {
          case Jump => fatNode
          case Normal => SequenceNode(newNode, data)
        }
      case EmptyNode => newNode
    }
  }

  def appendAst(fatNode: FatNode, normalNextPath: AST, jumpNextPath: AST): AST = {
    fatNode match {
      case BranchNode(condition, thenNode, elseNode) =>
        ITE(condition, appendAst(thenNode, normalNextPath, jumpNextPath), appendAst(elseNode, normalNextPath, jumpNextPath))
      case SequenceNode(next, data) =>
        Block(List(data.ast, appendAst(next, normalNextPath, jumpNextPath)))
      case LeafNode(data) =>
        data.breakOrContinue match {
          case Jump => Block(List(data.ast, jumpNextPath))
          case Normal => Block(List(data.ast, normalNextPath))
        }
      case EmptyNode => normalNextPath
    }
  }
}

sealed trait FatNode

case class BranchNode(condition: ExpressionTree, thenNode: FatNode, elseNode: FatNode) extends FatNode

case class SequenceNode(next: FatNode, data: Data) extends FatNode {
  assert(data.breakOrContinue == Normal)
}

case class LeafNode(data: Data) extends FatNode

object EmptyNode extends FatNode