package brbo.common.instrument

import brbo.common.TypeUtils.BrboType
import brbo.common.instrument.FileFormat.JAVA_FORMAT
import brbo.common.instrument.InstrumentUtils.{NewMethodInformation, appendSemiColon}
import brbo.common.{TargetMethod, TreeUtils}
import brbo.verification.BasicProcessor
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager

import scala.collection.JavaConverters._

object ChangeEntryNode {
  private val logger = LogManager.getLogger("brbo.common.instrument.ChangeEntryNode")

  /**
   *
   * @param inputMethod A method to be instrumented
   * @param entryTree   A command in the input method, which will be the entry command of the new method
   * @return A method that is almost the same as the input method, but with a different entry command
   */
  def changeEntryNode(inputMethod: TargetMethod, entryTree: StatementTree): TargetMethod = {
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
    val newSourceCode = InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(
      inputMethod,
      NewMethodInformation(Some(parameters), None, None, Nil, None, isAbstractClass = false, newMethodBody = s"{\n${newAst.print}\n}"),
      JAVA_FORMAT,
      indent = 2)
    BasicProcessor.getTargetMethod(inputMethod.fullQualifiedClassName, newSourceCode)
  }

  def treeToAst(tree: StatementTree): AST = {
    tree match {
      case tree if TreeUtils.isCommand(tree) => Command(tree)
      case tree: BlockTree => Block(tree.getStatements.asScala.toList.map(s => treeToAst(s)))
      case tree: IfTree => ITE(tree.getCondition, treeToAst(tree.getThenStatement), treeToAst(tree.getElseStatement))
      case tree: ForLoopTree =>
        val initializer = tree.getInitializer.asScala.toList.map(s => treeToAst(s))
        val body = tree.getStatement :: tree.getUpdate.asScala.toList
        Block(initializer :+ Loop(tree.getCondition, Block(body.map(s => treeToAst(s)))))
      case tree: WhileLoopTree => Loop(tree.getCondition, treeToAst(tree.getStatement))
      case _ => throw new Exception("Unexpected")
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
      }
    }
  }
}

sealed trait AST {
  def print: String
}

case class Block(statements: List[AST]) extends AST {
  override def print: String = s"{ ${statements.map(t => t.print).mkString(" ")} }"
}

case class Loop(condition: ExpressionTree, body: AST) extends AST {
  override def print: String = {
    val conditionString = {
      val string = condition.toString
      if (string.startsWith("(") && string.endsWith(")")) string
      else s"($string)"
    }
    s"while $conditionString ${body.print}"
  }
}

case class ITE(condition: ExpressionTree, thenAst: AST, elseAst: AST) extends AST {
  override def print: String = {
    val conditionString = {
      val string = condition.toString
      if (string.startsWith("(") && string.endsWith(")")) string
      else s"($string)"
    }
    s"if $conditionString ${thenAst.print} else ${elseAst.print}"
  }
}

case class Command(statement: StatementTree) extends AST {
  assert(TreeUtils.isCommand(statement))

  override def print: String = {
    // If x is a local variable of the original program, ensure x is never declared again in the body of the new program,
    // by not declaring variables in the body of the new program
    val string = {
      statement match {
        case variableTree: VariableTree => s"${variableTree.getName.toString} = ${variableTree.getInitializer}"
        case _ => statement.toString
      }
    }
    appendSemiColon(string)
  }
}

case object Return extends AST {
  // To avoid "unreachable statement" error from javac, we can use `if (true) return;`
  override def print: String = "if (true) return;"
}
