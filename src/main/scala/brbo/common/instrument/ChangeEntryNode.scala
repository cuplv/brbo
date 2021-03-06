package brbo.common.instrument

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.TypeUtils.BrboType
import brbo.common.instrument.FileFormat.JAVA_FORMAT
import brbo.common.instrument.InstrumentUtils.{NewMethodInformation, appendSemiColon}
import brbo.common.instrument.JumpOrNormalOrExit.{Exit, Jump, JumpOrNormalOrExit, Normal}
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
  def changeEntryNode(inputMethod: TargetMethod,
                      entryTree: StatementTree,
                      preservedUpdates: Set[StatementTree],
                      testMode: Boolean): TargetMethod = {
    assert(TreeUtils.isCommand(entryTree))
    val entryTreeAst = treeToAst(entryTree)

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
                val toAppend = handleBreakContinue(treeToAst(statements(i)), enclosingLoop.map(s => treeToAst(s)))
                node = FatNode.append(node, toAppend)
              }
            }
            i = i + 1
          }

          val normalNextPath = {
            val additionalUpdates: List[AST] = {
              // Check if the enclosing tree of this block tree is a for loop. If yes, then append the updates
              if (enclosingTrees.size > 3) {
                enclosingTrees(enclosingTrees.size - 3) match {
                  case forLoopTree: ForLoopTree => forLoopTree.getUpdate.asScala.map(u => treeToAst(u)).toList
                  case _ => Nil
                }
              }
              else Nil
            }
            val nextPaths = handleNextPaths(tree)
            additionalUpdates match {
              case Nil => nextPaths
              case _ => Block(additionalUpdates :+ nextPaths)
            }
          }
          // Jump target of the enclosing block tree
          val jumpNextPath = enclosingLoop match {
            case Some(loop) =>
              FatNode.appendAst(processLoop(loop, entryTreeAst), handleNextPaths(loop), BadJumpTarget)
            case None => Empty
          }
          FatNode.appendAst(node, normalNextPath, jumpNextPath)
        case tree: IfTree => handleNextPaths(tree)
        case tree@(_: WhileLoopTree | _: ForLoopTree) =>
          FatNode.appendAst(processLoop(tree, entryTreeAst), handleNextPaths(tree), BadJumpTarget)
        case _ => logger.fatal(s"$enclosingTree (${enclosingTree.getClass})"); throw new Exception("Unexpected")
      }
    }

    val newAst = Block(List(treeToAst(entryTree), handleNextPaths(entryTree)))
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
        case _ => throw new Exception("Unexpected")
      }
    }
  }

  private def processLoop(loop: StatementTree, toBeSubstituted: AST): FatNode = {
    val loopAST: AST = loop match {
      case forLoopTree: ForLoopTree =>
        Loop(forLoopTree.getCondition, Block(treeToAst(forLoopTree.getStatement) :: forLoopTree.getUpdate.asScala.toList.map(t => treeToAst(t))))
      case whileLoopTree: WhileLoopTree => treeToAst(whileLoopTree)
      case _ => throw new Exception("Unexpected")
    }
    val newLoopAST: AST = substitute(loopAST, toBeSubstituted, Return)
    handleBreakContinue(newLoopAST, None)
  }

  /**
   *
   * @return ALl paths (of the original program) that begin and end with the current tree,
   *         along with the "direction" of the immediate next control flow
   */
  def handleBreakContinue(currentTree: AST, enclosingLoop: Option[AST]): FatNode = {
    if (currentTree == null)
      return EmptyNode

    currentTree match {
      case Block(statements) =>
        var node: FatNode = EmptyNode
        var i = 0
        while (i < statements.size) {
          val toAppend = handleBreakContinue(statements(i), enclosingLoop)

          node = FatNode.append(node, toAppend)
          i = i + 1
        }
        node
      case Loop(condition, body) =>
        val newBody = {
          val newBody = handleBreakContinue(body, Some(currentTree)) // Possible to exit inside loop body!
          FatNode.toAST(newBody)
        }
        // TODO: The next control flow is not necessarily normal because it is possible to exit inside loop body
        LeafNode(Data(Loop(condition, newBody), Normal))
      case ITE(condition, thenAst, elseAst) =>
        BranchNode(condition,
          handleBreakContinue(thenAst, enclosingLoop), handleBreakContinue(elseAst, enclosingLoop))
      case Command(statement) =>
        statement match {
          case _@(_: BreakTree | _: ContinueTree) =>
            enclosingLoop match {
              case Some(loop) =>
                loop match {
                  case Loop(_, _) => LeafNode(Data(Empty, Jump)) // This corresponds to a while loop
                  case Block(statements) =>
                    statements.last match {
                      case Loop(_, _) => LeafNode(Data(Empty, Jump)) // This corresponds to a for loop. See `treetoAST`
                      case _ => throw new Exception("Unexpected")
                    }
                  case _ => throw new Exception("Unexpected")
                }
              case None => throw new Exception("Unexpected")
            }
          case _: ReturnTree => LeafNode(Data(currentTree, Exit))
          case _ => LeafNode(Data(currentTree, Normal))
        }
      case Return => LeafNode(Data(Return, Exit))
      case Empty => LeafNode(Data(currentTree, Normal))
      case _ => throw new Exception("Unexpected")
    }
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
    val bodyString = {
      val raw = body.print(preserveDeclaration, preservedUpdates)
      if (raw.startsWith("{") && raw.endsWith("}")) raw
      else s"{$raw}"
    }
    s"while $conditionString $bodyString"
  }
}

case class ITE(condition: ExpressionTree, thenAst: AST, elseAst: AST) extends AST {
  override def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String = {
    val conditionString = {
      val string = condition.toString
      if (string.startsWith("(") && string.endsWith(")")) string
      else s"($string)"
    }
    val thenString = {
      val raw = thenAst.print(preserveDeclaration, preservedUpdates)
      if (raw.startsWith("{") && raw.endsWith("}")) raw
      else s"{$raw}"
    }
    val elseString = {
      val raw = elseAst.print(preserveDeclaration, preservedUpdates)
      if (raw.startsWith("{") && raw.endsWith("}")) raw
      else s"{$raw}"
    }
    s"if $conditionString $thenString else $elseString"
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
  override def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String = "{return;}"
}

case object Empty extends AST {
  override def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String = "{;}"
}

case object BadJumpTarget extends AST {
  override def print(preserveDeclaration: Boolean, preservedUpdates: Set[StatementTree]): String = throw new Exception("Unexpected")
}

object JumpOrNormalOrExit extends Enumeration {
  type JumpOrNormalOrExit = Value
  val Jump, Normal, Exit = Value
}

case class Data(ast: AST, breakOrContinue: JumpOrNormalOrExit)

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
          case Exit => fatNode
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
          case Jump =>
            assert(jumpNextPath != BadJumpTarget)
            Block(List(data.ast, jumpNextPath))
          case Normal => Block(List(data.ast, normalNextPath))
          case Exit => data.ast
        }
      case EmptyNode => normalNextPath
    }
  }

  def toAST(fatNode: FatNode): AST = {
    fatNode match {
      case BranchNode(condition, thenNode, elseNode) => ITE(condition, toAST(thenNode), toAST(elseNode))
      case SequenceNode(next, data) => Block(List(data.ast, toAST(next)))
      case LeafNode(data) => data.ast
      case EmptyNode => Empty
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