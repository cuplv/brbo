package brbo.common

import brbo.common.Instrument.GhostVariable.{Counter, Delta, GhostVariable, Resource}
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.block.Block
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node, NumericalAdditionNode}

import scala.collection.JavaConverters._

object Instrument {
  private val logger = LogManager.getLogger("common.Instrument")
  private val deltaVariablePattern = """D\d*""".r
  private val resourceVariablePattern = """R\d*""".r
  private val counterVariablePattern = """C\d*""".r
  private val INDENT = 2

  object GhostVariable extends Enumeration {
    type GhostVariable = Value
    val Resource, Delta, Counter = Value
  }

  def isGhostVariable(identifier: String, typ: GhostVariable): Boolean = {
    val pattern = typ match {
      case Resource => resourceVariablePattern
      case Delta => deltaVariablePattern
      case Counter => counterVariablePattern
    }
    identifier match {
      case pattern() => true
      case _ => false
    }
  }

  case class GhostVariableUpdateNode(identifier: String, update: Node) {
    override def toString: String = s"$identifier = $identifier + $update"
  }

  case class GhostVariableUpdateTree(identifier: String, update: Tree) {
    override def toString: String = s"$identifier = $identifier + $update"
  }

  def extractGhostVariableFromAssignment(cfgNode: Node, typ: GhostVariable): Option[GhostVariableUpdateNode] = {
    cfgNode match {
      case node: AssignmentNode =>
        // Must be in the form of g = g + e
        if (isGhostVariable(node.getTarget.toString, typ)) {
          val ghostVariable = node.getTarget.toString
          node.getExpression match {
            case rhs: NumericalAdditionNode =>
              if (rhs.getLeftOperand.toString == ghostVariable) Some(GhostVariableUpdateNode(ghostVariable, rhs.getRightOperand))
              else {
                logger.warn(s"Assignment to ghost variable `$ghostVariable` is not in the form of `$ghostVariable = $ghostVariable + e`!")
                None
              }
            case _ => None
          }
        }
        else None
      case _ => None
    }
  }

  def extractGhostVariableFromAssignment(tree: Tree, typ: GhostVariable): Option[GhostVariableUpdateTree] = {
    tree match {
      case tree: AssignmentTree =>
        // Must be in the form of g = g + e
        if (isGhostVariable(tree.getVariable.toString, typ)) {
          val ghostVariable = tree.getVariable.toString
          tree.getExpression match {
            case rhs: BinaryTree =>
              if (rhs.getLeftOperand.toString == ghostVariable) Some(GhostVariableUpdateTree(ghostVariable, rhs.getRightOperand))
              else {
                logger.warn(s"Assignment to ghost variable `$ghostVariable` is not in the form of `$ghostVariable = $ghostVariable + e`!")
                None
              }
            case _ => None
          }
        }
        else None
      case _ => None
    }
  }

  def substituteAllAtomicStatementsWith(tree: Tree, instrumentation: Map[Node, String], indent: Int, cfg: ControlFlowGraph): String = {
    val spaces = " " * indent
    ???
  }

  /**
   *
   * @param shouldInstrument determine if a node should be instrumented
   * @param howToInstrument  determine how to instrument a tree (without indents and semicolon), when its corresponding nodes ALL satisfy the predicate
   */
  case class AtomicStatementInstrumentation(shouldInstrument: Node => Boolean, howToInstrument: Tree => String) {
    def instrument(tree: Tree, state: InstrumentState, indent: Int, cfg: ControlFlowGraph): InstrumentResult = {
      val spaces = " " * indent
      val original = InstrumentResult(s"$spaces${tree.toString};", InstrumentState(state.needInstrument, hasInstrumented = false))
      if (!state.needInstrument) {
        return original
      }
      tree match {
        case atomicStatement@(_: ExpressionStatementTree | _: VariableTree |
                              _: AssertTree | _: BreakTree | _: ContinueTree | _: EmptyStatementTree | _: ReturnTree) =>
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
              case _ =>
                val nodes = cfg.getNodesCorrespondingToTree(tree)
                if (nodes != null) nodes.asScala.toSet
                else Set.empty
            }
          }
          if (cfgNodes.isEmpty) original
          else if (cfgNodes.forall(node => shouldInstrument(node))) {
            logger.debug(s"AST `$tree` is mapped to nodes `$cfgNodes` and all nodes satisfy instrumentation")
            InstrumentResult(
              s"$spaces${howToInstrument(tree)};",
              InstrumentState(state.needInstrument, hasInstrumented = true)
            )
          }
          else original
        case _ => errorInstrumentResult(s"Tree `$tree` is not an atomic statement!")
      }
    }
  }

  /**
   *
   * @param result the instrumented string
   * @param state  true only if instrumentation happened during constructing result
   */
  case class InstrumentResult(result: String, state: InstrumentState)

  case class InstrumentState(needInstrument: Boolean, hasInstrumented: Boolean) {
    assert(if (hasInstrumented) needInstrument else true)
  }

  private def errorInstrumentResult(errorMessage: String): InstrumentResult = throw new RuntimeException(errorMessage)

  def substituteAtMostOneAtomicStatement(tree: Tree,
                                         instrumentation: AtomicStatementInstrumentation,
                                         indent: Int,
                                         cfg: ControlFlowGraph,
                                         getLineNumber: Tree => Int): InstrumentResult = {

    def substituteAtMostOneAtomicStatementInSequences(statementTrees: Iterable[Tree], state: InstrumentState, indent2: Int): InstrumentResult = {
      statementTrees.foldLeft(InstrumentResult("", state))({
        (acc, statementTree) =>
          if (acc.state.hasInstrumented) InstrumentResult(acc + "\n" + statementTree.toString, acc.state)
          else {
            val result = substituteAtMostOneAtomicStatementHelper(statementTree, state, indent2)
            InstrumentResult(acc + "\n" + result.result, result.state)
          }
      })
    }

    def substituteAtMostOneAtomicStatementHelper(tree2: Tree, state: InstrumentState, indent2: Int): InstrumentResult = {
      val spaces = " " * indent2
      tree2 match {
        case _: AssertTree => instrumentation.instrument(tree2, state, indent2, cfg)
        case tree3: BlockTree =>
          val result = substituteAtMostOneAtomicStatementInSequences(tree3.getStatements.asScala, state, indent2)
          InstrumentResult(s"$spaces{${result.result}\n$spaces}", result.state)
        case _: BreakTree => instrumentation.instrument(tree2, state, indent2, cfg)
        case _: ClassTree => errorInstrumentResult(s"Cannot instrument a class tree at line ${getLineNumber(tree2)}")
        case _: ContinueTree => instrumentation.instrument(tree2, state, indent2, cfg)
        case tree3: DoWhileLoopTree =>
          val result = substituteAtMostOneAtomicStatementHelper(tree3.getStatement, state, indent2)
          InstrumentResult(
            s"${spaces}do\n${result.result}\n${spaces}while (${tree3.getCondition.toString});",
            result.state)
        case _: EmptyStatementTree => instrumentation.instrument(tree2, state, indent2, cfg)
        case _: EnhancedForLoopTree => errorInstrumentResult(s"Not yet support enhanced for loop at line ${getLineNumber(tree2)}")
        /*s"${spaces}for (${tree.getVariable} : ${tree.getExpression}) {\n" +
          printTreeInstrumentedAtBlock(tree.getStatement, deltaVariable, block, indent + INDENT) +
          s"\n$spaces}"*/
        case _: ExpressionStatementTree => instrumentation.instrument(tree2, state, indent2, cfg)
        /*tree.getExpression match {
          case _@(_: UnaryTree | _: AssignmentTree) =>
            spaces + tree.toString + resetStatement // Avoid an extra semicolon
          case _ =>
            spaces + tree.toString + ";" + resetStatement
        }*/
        case tree3: ForLoopTree =>
          val result1 = substituteAtMostOneAtomicStatementInSequences(tree3.getInitializer.asScala, state, indent2 + INDENT)
          val result2 = {
            val body = tree3.getStatement match {
              case blockTree: BlockTree =>
                substituteAtMostOneAtomicStatementInSequences(blockTree.getStatements.asScala, result1.state, indent2 + INDENT + INDENT)
              case _ => errorInstrumentResult("Unreachable")
            }
            val updates = substituteAtMostOneAtomicStatementInSequences(tree3.getUpdate.asScala, body.state, indent2 + INDENT + INDENT)
            val extraIndent = " " * INDENT
            InstrumentResult(
              s"$spaces${extraIndent}while (${tree3.getCondition}) {" +
                s"${body.result}${updates.result}\n$spaces$extraIndent}"
              , body.state)
          }
          InstrumentResult(
            s"$spaces{// For loop${result1.result}${result2.result}\n$spaces}",
            result2.state
          )
        case tree3: IfTree =>
          val result1 = substituteAtMostOneAtomicStatementHelper(tree3.getThenStatement, state, indent2 + INDENT)
          val result2 = substituteAtMostOneAtomicStatementHelper(tree3.getElseStatement, result1.state, indent2 + INDENT)
          InstrumentResult(
            s"${spaces}if (${tree3.getCondition}) {\n${result1.result}$spaces" +
              s"} else {\n${result2.result}\n$spaces}",
            result2.state)
        case tree3: LabeledStatementTree =>
          val result = substituteAtMostOneAtomicStatementHelper(tree3.getStatement, state, indent2)
          InstrumentResult(s"$spaces${tree3.getLabel.toString}:\n${result.result}", result.state)
        case _: ReturnTree => instrumentation.instrument(tree2, state, indent2, cfg)
        case _: SwitchTree => errorInstrumentResult(s"Not yet support switch tree at line ${getLineNumber(tree2)}")
        // s"${spaces}switch (${tree.getExpression}) {" + s"\n$spaces}"
        case _: SynchronizedTree => errorInstrumentResult(s"Not yet support synchronized tree at line ${getLineNumber(tree2)}")
        case _: ThrowTree => errorInstrumentResult(s"Not yet support throw tree at line ${getLineNumber(tree2)}")
        case _: TryTree => errorInstrumentResult(s"Not yet support try tree at line ${getLineNumber(tree2)}")
        case _: VariableTree => instrumentation.instrument(tree2, state, indent2, cfg)
        case tree: WhileLoopTree =>
          val result = substituteAtMostOneAtomicStatementHelper(tree.getStatement, state, indent2)
          InstrumentResult(s"${spaces}while (${tree.getCondition})\n${result.result}", result.state)
      }
    }

    substituteAtMostOneAtomicStatementHelper(tree, InstrumentState(needInstrument = true, hasInstrumented = false), indent)
  }

  // Insert d = 0 in the AST that maps to targetBlock
  def substituteAllAtomicStatementsWith(tree: Tree, deltaVariable: String, targetBlock: Block, indent: Int, cfg: ControlFlowGraph): String = {
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
          (acc, statementTree) => acc + "\n" + substituteAllAtomicStatementsWith(statementTree, deltaVariable, targetBlock, indent + INDENT, cfg)
        ) + s"\n$spaces}"
      case tree: BreakTree => spaces + tree.toString + ";"
      case _: ClassTree => assert(assertion = false, "Unreachable"); ""
      case tree: ContinueTree => spaces + tree.toString + ";"
      case tree: DoWhileLoopTree =>
        s"${spaces}do\n" +
          substituteAllAtomicStatementsWith(tree.getStatement, deltaVariable, targetBlock, indent, cfg) +
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
          (acc, statementTree) => acc + "\n" + substituteAllAtomicStatementsWith(statementTree, deltaVariable, targetBlock, indent + INDENT, cfg)
        ) + "\n"
        val part2 = {
          val updates = tree.getUpdate.asScala.foldLeft("")(
            (acc, statementTree) => acc + "\n" + substituteAllAtomicStatementsWith(statementTree, deltaVariable, targetBlock, indent + INDENT + INDENT, cfg)
          )
          val extraIndent = " " * INDENT
          tree.getStatement match {
            case blockTree: BlockTree =>
              val body = blockTree.getStatements.asScala.foldLeft("")(
                (acc, statementTree) => acc + "\n" + substituteAllAtomicStatementsWith(statementTree, deltaVariable, targetBlock, indent + INDENT + INDENT, cfg)
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
          substituteAllAtomicStatementsWith(tree.getThenStatement, deltaVariable, targetBlock, indent + INDENT, cfg) +
          s"$spaces} else {\n" +
          substituteAllAtomicStatementsWith(tree.getElseStatement, deltaVariable, targetBlock, indent + INDENT, cfg) +
          s"\n$spaces}"
      case tree: LabeledStatementTree => spaces + "\n" + substituteAllAtomicStatementsWith(tree.getStatement, deltaVariable, targetBlock, indent, cfg)
      case tree: ReturnTree => spaces + tree.toString + ";"
      case _: SwitchTree => assert(assertion = false, "TODO"); ""
      // s"${spaces}switch (${tree.getExpression}) {" + s"\n$spaces}"
      case _: SynchronizedTree => assert(assertion = false, "Unreachable"); ""
      case _: ThrowTree => assert(assertion = false, "Unreachable"); ""
      case _: TryTree => assert(assertion = false, "Unreachable"); ""
      case tree: VariableTree => spaces + tree.toString + ";" + generateResetStatementGivenAtomicStatement(tree)
      case tree: WhileLoopTree =>
        s"${spaces}while (${tree.getCondition})\n" +
          substituteAllAtomicStatementsWith(tree.getStatement, deltaVariable, targetBlock, indent, cfg)
    }
  }
}
