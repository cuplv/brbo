package brbo.common.instrument

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.TypeUtils.BrboType
import brbo.common.instrument.FileFormat.JAVA_FORMAT
import brbo.common.instrument.InstrumentUtils.{NewMethodInformation, appendSemiColon}
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
  def changeEntryNode(inputMethod: TargetMethod, entryTree: StatementTree, preservedUpdates: Set[StatementTree]): TargetMethod = {
    assert(TreeUtils.isCommand(entryTree))
    val entryTreeAst = treeToAst(entryTree)

    /**
     *
     * @return All paths of the original program (expressed with an AST) that start from the immediate next control location of the current tree
     */
    def nextPaths(currentTree: StatementTree): AST = {
      val enclosingTrees: List[StatementTree] = {
        val enclosingTrees = TreeUtils.getEnclosingStatementTrees(inputMethod.getPath(currentTree))
        assert(enclosingTrees.last == currentTree)
        enclosingTrees
      }
      if (enclosingTrees.size == 1) return Return

      val enclosingTree = enclosingTrees(enclosingTrees.size - 2)
      enclosingTree match {
        case tree: BlockTree =>
          var prepend: List[AST] = Nil
          var foundEntryTree = false
          tree.getStatements.asScala.toList.foreach({
            statement =>
              if (statement == currentTree) {
                foundEntryTree = true
              }
              else {
                if (foundEntryTree) prepend = prepend :+ treeToAst(statement)
              }
          })
          Block(prepend :+ nextPaths(tree))
        case tree: IfTree => nextPaths(tree)
        case tree: WhileLoopTree =>
          Block(List(substitute(treeToAst(enclosingTree), entryTreeAst, Return), nextPaths(tree)))
        case tree: ForLoopTree =>
          val loopBody = Loop(tree.getCondition, Block(treeToAst(tree.getStatement) :: tree.getUpdate.asScala.toList.map(t => treeToAst(t))))
          Block(List(substitute(loopBody, entryTreeAst, Return), nextPaths(tree)))
        case _ => println(enclosingTree, enclosingTree.getClass); throw new Exception("Unexpected")
      }
    }

    val newAst = nextPaths(entryTree)
    val parameters = (inputMethod.inputVariables ++ inputMethod.localVariables).toList.map(pair => BrboType.variableDeclaration(pair._1, pair._2)).sorted.mkString(", ")
    val newMethodBody = s"{\n${newAst.print(preserveDeclaration = false, preservedUpdates)}\n}"
    val newSourceCode = InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(
      inputMethod,
      NewMethodInformation(Some(parameters), None, None, Nil, None, isAbstractClass = false, newMethodBody),
      JAVA_FORMAT,
      indent = 2)
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