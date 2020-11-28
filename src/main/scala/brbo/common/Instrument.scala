package brbo.common

import com.sun.source.tree._
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.block.Block
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node, NumericalAdditionNode}

import scala.collection.JavaConverters._

object Instrument {
  private val logger = LogManager.getLogger("common.Instrument")
  private val deltaVariablePattern = """D\d*""".r

  def isDeltaVariable(identifier: String): Boolean = {
    identifier match {
      case deltaVariablePattern() => true
      case _ => false
    }
  }

  def extractDeltaVariableFromAssignment(cfgNode: Node): Option[String] = {
    cfgNode match {
      case node: AssignmentNode =>
        // Must be in the form of d = d + e
        if (isDeltaVariable(node.getTarget.toString)) {
          val deltaVariable = node.getTarget.toString
          node.getExpression match {
            case node: NumericalAdditionNode =>
              if (node.getLeftOperand.toString == deltaVariable) Some(deltaVariable)
              else {
                logger.warn(s"Assignment to delta variable `$deltaVariable` is not in the form of `$deltaVariable = $deltaVariable + e`!")
                None
              }
            case _ => None
          }
        }
        else None
      case _ => None
    }
  }

  // Insert d = 0 in the AST that maps to targetBlock
  def instrumentTreeByConvertingToString(tree: Tree, deltaVariable: String, targetBlock: Block, indent: Int, cfg: ControlFlowGraph): String = {
    val INDENT = 2
    val spaces = " " * indent

    def generateResetStatementGivenAtomicStatement(tree: Tree): String = {
      tree match {
        case atomicStatement@(_: ExpressionStatementTree | _: VariableTree) =>
          val cfgNodes: Set[Node] = {
            atomicStatement match {
              case expressionStatementTree: ExpressionStatementTree =>
                expressionStatementTree.getExpression match {
                  case unaryTree: UnaryTree =>
                    cfg.getUnaryAssignNodeLookup.asScala.get(unaryTree) match {
                      case Some(assignmentNode) => Set(assignmentNode)
                      case None => Set.empty
                    }
                  case expressionTree@_ => cfg.getNodesCorrespondingToTree(expressionTree).asScala.toSet
                }
              case variableTree: VariableTree => cfg.getNodesCorrespondingToTree(variableTree).asScala.toSet
            }
          }
          if (cfgNodes.subsetOf(targetBlock.getNodes.asScala.toSet)) {
            logger.debug(s"AST `$tree` is mapped to nodes `$cfgNodes` in target block `$targetBlock`")
            s"\n$spaces$deltaVariable = 0;"
          }
          else ""
        case _ => assert(assertion = false, s"Tree `$tree` is not an atomic statement!"); ""
      }
    }

    tree match {
      case tree: AssertTree => spaces + tree.toString + ";"
      case tree: BlockTree =>
        tree.getStatements.asScala.foldLeft(s"$spaces{")(
          (acc, statementTree) => acc + "\n" + instrumentTreeByConvertingToString(statementTree, deltaVariable, targetBlock, indent + INDENT, cfg)
        ) + s"\n$spaces}"
      case tree: BreakTree => spaces + tree.toString + ";"
      case _: ClassTree => assert(assertion = false, "Unreachable"); ""
      case tree: ContinueTree => spaces + tree.toString + ";"
      case tree: DoWhileLoopTree =>
        s"${spaces}do\n" +
          instrumentTreeByConvertingToString(tree.getStatement, deltaVariable, targetBlock, indent, cfg) +
          s"\n${spaces}while (${tree.getCondition.toString});"
      case _: EmptyStatementTree => spaces
      case _: EnhancedForLoopTree => assert(assertion = false, "Unreachable"); ""
      /*s"${spaces}for (${tree.getVariable} : ${tree.getExpression}) {\n" +
        printTreeInstrumentedAtBlock(tree.getStatement, deltaVariable, block, indent + INDENT) +
        s"\n$spaces}"*/
      case tree: ExpressionStatementTree =>
        // Insert d = 0 at potentially multiple places in the target basic block.
        // This neither affects the correctness nor the way to construct bounds.
        val resetStatement = generateResetStatementGivenAtomicStatement(tree)
        tree.getExpression match {
          case _@(_: UnaryTree | _: AssignmentTree) =>
            spaces + tree.toString + resetStatement // Avoid an extra semicolon
          case _ =>
            spaces + tree.toString + ";" + resetStatement
        }
      case tree: ForLoopTree =>
        val part1 = tree.getInitializer.asScala.foldLeft(s"$spaces{// For loop")(
          (acc, statementTree) => acc + "\n" + instrumentTreeByConvertingToString(statementTree, deltaVariable, targetBlock, indent + INDENT, cfg)
        ) + "\n"
        val part2 = {
          val updates = tree.getUpdate.asScala.foldLeft("")(
            (acc, statementTree) => acc + "\n" + instrumentTreeByConvertingToString(statementTree, deltaVariable, targetBlock, indent + INDENT + INDENT, cfg)
          )
          val extraIndent = " " * INDENT
          tree.getStatement match {
            case blockTree: BlockTree =>
              val body = blockTree.getStatements.asScala.foldLeft("")(
                (acc, statementTree) => acc + "\n" + instrumentTreeByConvertingToString(statementTree, deltaVariable, targetBlock, indent + INDENT + INDENT, cfg)
              )
              s"$spaces${extraIndent}while (${tree.getCondition}) {" +
                body + updates +
                s"\n$spaces$extraIndent}"
            case _ => assert(assertion = false, "Unreachable"); ""
          }
        }
        val part3 = s"\n$spaces}"
        part1 + part2 + part3
      case tree: IfTree =>
        s"${spaces}if (${tree.getCondition}) {\n" +
          instrumentTreeByConvertingToString(tree.getThenStatement, deltaVariable, targetBlock, indent + INDENT, cfg) +
          s"$spaces} else {\n" +
          instrumentTreeByConvertingToString(tree.getElseStatement, deltaVariable, targetBlock, indent + INDENT, cfg) +
          s"\n$spaces}"
      case tree: LabeledStatementTree => spaces + "\n" + instrumentTreeByConvertingToString(tree.getStatement, deltaVariable, targetBlock, indent, cfg)
      case tree: ReturnTree => spaces + tree.toString + ";"
      case _: SwitchTree => assert(assertion = false, "TODO"); ""
      // s"${spaces}switch (${tree.getExpression}) {" + s"\n$spaces}"
      case _: SynchronizedTree => assert(assertion = false, "Unreachable"); ""
      case _: ThrowTree => assert(assertion = false, "Unreachable"); ""
      case _: TryTree => assert(assertion = false, "Unreachable"); ""
      case tree: VariableTree => spaces + tree.toString + ";" + generateResetStatementGivenAtomicStatement(tree)
      case tree: WhileLoopTree =>
        s"${spaces}while (${tree.getCondition})\n" +
          instrumentTreeByConvertingToString(tree.getStatement, deltaVariable, targetBlock, indent, cfg)
    }
  }
}
