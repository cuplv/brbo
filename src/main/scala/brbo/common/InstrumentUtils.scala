package brbo.common

import brbo.common.FileFormat.{C_FORMAT, FileFormat, JAVA_FORMAT}
import brbo.common.InstrumentUtils.GhostVariable.{Counter, Delta, GhostVariable, Resource}
import brbo.common.InstrumentUtils.InstrumentMode.{ALL, AT_MOST_ONCE, InstrumentMode}
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.block.Block
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node, NumericalAdditionNode}

import scala.collection.JavaConverters._

object InstrumentUtils {
  private val logger = LogManager.getLogger("brbo.common.Instrument")
  private val deltaVariablePattern = """D\d*""".r
  private val resourceVariablePattern = """R\d*""".r
  private val counterVariablePattern = """C\d*""".r
  val INDENT = 2

  val defaultDeltaVariable = "D100"
  val defaultResourceAssignment: AtomicStatementInstrumentation =
    AtomicStatementInstrumentation( // Replace `R = R + e` with `d = d + e`
      {
        node: Node => InstrumentUtils.extractGhostVariableFromAssignment(node, Resource).nonEmpty
      },
      {
        case expressionStatement: ExpressionStatementTree =>
          expressionStatement.getExpression match {
            case assignmentTree: AssignmentTree =>
              val update = InstrumentUtils.extractGhostVariableFromAssignment(assignmentTree, Resource)
              s"$defaultDeltaVariable = $defaultDeltaVariable + ${update.get.update}"
            case unaryTree: UnaryTree =>
              unaryTree.getKind match {
                case Tree.Kind.PREFIX_DECREMENT | Tree.Kind.POSTFIX_DECREMENT => s"$defaultDeltaVariable = $defaultDeltaVariable - 1"
                case Tree.Kind.PREFIX_INCREMENT | Tree.Kind.POSTFIX_INCREMENT => s"$defaultDeltaVariable = $defaultDeltaVariable + 1"
                case tree@_ => throw new RuntimeException(s"Unknown unary tree operator `$tree`")
              }
            case tree@_ => throw new RuntimeException(s"Instrumenting non assignment statement `$tree` (Kind: ${tree.getKind})")
          }
        case tree@_ => throw new RuntimeException(s"Instrumenting non expression statement `$tree` (Kind: ${tree.getKind})")
      })
  val removeResourceAssignment: AtomicStatementInstrumentation =
    AtomicStatementInstrumentation( // Replace `R = R + e` with `;`
      {
        node: Node => InstrumentUtils.extractGhostVariableFromAssignment(node, Resource).nonEmpty
      },
      {
        case expressionStatement: ExpressionStatementTree =>
          expressionStatement.getExpression match {
            case assignmentTree: AssignmentTree => ";"
            case unaryTree: UnaryTree =>
              unaryTree.getKind match {
                case Tree.Kind.PREFIX_DECREMENT | Tree.Kind.POSTFIX_DECREMENT => ";"
                case Tree.Kind.PREFIX_INCREMENT | Tree.Kind.POSTFIX_INCREMENT => ";"
                case tree@_ => throw new RuntimeException(s"Unknown unary tree operator `$tree`")
              }
            case tree@_ => throw new RuntimeException(s"Instrumenting non assignment statement `$tree` (Kind: ${tree.getKind})")
          }
        case tree@_ => throw new RuntimeException(s"Instrumenting non expression statement `$tree` (Kind: ${tree.getKind})")
      })

  object GhostVariable extends Enumeration {
    type GhostVariable = Value
    val Resource, Delta, Counter = Value
  }

  object InstrumentMode extends Enumeration {
    type InstrumentMode = Value
    val AT_MOST_ONCE, ALL = Value
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

  def extractGhostVariableFromAssignment(cfgNode: Node, types: Iterable[GhostVariable]): Option[GhostVariableUpdateNode] = {
    types.foldLeft(None: Option[GhostVariableUpdateNode])({
      (acc, typ) =>
        acc match {
          case Some(_) => acc
          case None => extractGhostVariableFromAssignment(cfgNode, typ)
        }
    })
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

  def substituteAtomicStatements(tree: Tree,
                                 instrumentation: AtomicStatementInstrumentation,
                                 indent: Int,
                                 cfg: ControlFlowGraph,
                                 getLineNumber: Tree => Int,
                                 instrumentMode: InstrumentMode): InstrumentResult = {
    instrumentMode match {
      case ALL => substituteAtomicStatementHelper(tree, InstrumentState(needInstrument = true, hasInstrumented = false, ALL, instrumentation, cfg, getLineNumber), indent)
      case AT_MOST_ONCE => substituteAtomicStatementHelper(tree, InstrumentState(needInstrument = true, hasInstrumented = false, AT_MOST_ONCE, instrumentation, cfg, getLineNumber), indent)
    }
  }

  /**
   *
   * @param shouldInstrument determine if a node's corresponding AST should be instrumented
   * @param howToInstrument  determine how to instrument a tree (without indents and semicolon), when its corresponding nodes ALL satisfy the predicate
   */
  case class AtomicStatementInstrumentation(shouldInstrument: Node => Boolean, howToInstrument: Tree => String) {
    def instrument(tree: Tree, state: InstrumentState, indent: Int, cfg: ControlFlowGraph): InstrumentResult = {
      val spaces = " " * indent
      val original = InstrumentResult(s"$spaces${tree.toString};", state.updateHasInstrumented(false))
      if (!state.needInstrument) {
        assert(!state.hasInstrumented)
        return original
      }
      if (state.hasInstrumented && state.instrumentMode == AT_MOST_ONCE) {
        assert(state.needInstrument)
        val optionalSemicolon = {
          val noSemiColon =
            tree match {
              case expressionStatementTree: ExpressionStatementTree =>
                expressionStatementTree.getExpression match {
                  case _@(_: UnaryTree | _: AssignmentTree) => true
                  case _ => false
                }
              case _ => false
            }
          if (noSemiColon) "" else ";"
        }
        return InstrumentResult(s"$spaces${tree.toString}$optionalSemicolon", state)
      }
      tree match {
        case atomicStatement@(_: ExpressionStatementTree | _: VariableTree |
                              _: AssertTree | _: BreakTree | _: ContinueTree | _: EmptyStatementTree | _: ReturnTree) =>
          val cfgNodes: Set[Node] = {
            atomicStatement match {
              case expressionStatementTree: ExpressionStatementTree =>
                expressionStatementTree.getExpression match {
                  case unaryTree: UnaryTree => cfg.getUnaryAssignNodeLookup.asScala.get(unaryTree).toSet
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
            logger.debug(s"AST `$tree` is instrumented into `${howToInstrument(tree)}`, because its corresponding nodes `$cfgNodes` all satisfy the guard of instrumentation")
            InstrumentResult(
              s"$spaces${howToInstrument(tree)};",
              state.updateHasInstrumented(true)
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
  case class InstrumentResult(result: String, state: InstrumentState) {
    assert(!result.contains("InstrumentResult"))
  }

  case class InstrumentState(needInstrument: Boolean,
                             hasInstrumented: Boolean, // The only mutable thing in a state
                             instrumentMode: InstrumentMode,
                             instrumentation: AtomicStatementInstrumentation,
                             cfg: ControlFlowGraph,
                             getLineNumber: Tree => Int) {
    assert(if (hasInstrumented) needInstrument else true)

    def updateHasInstrumented(newHasInstrumented: Boolean): InstrumentState = {
      InstrumentState(needInstrument, newHasInstrumented, instrumentMode, instrumentation, cfg, getLineNumber)
    }
  }

  private def errorInstrumentResult(errorMessage: String): InstrumentResult = throw new RuntimeException(errorMessage)

  private def substituteAtomicStatementInSequences(statementTrees: Iterable[Tree], state: InstrumentState, indent: Int): InstrumentResult = {
    statementTrees.foldLeft(InstrumentResult("", state))({
      (acc, statementTree) =>
        val result = substituteAtomicStatementHelper(statementTree, acc.state, indent)
        InstrumentResult(acc.result + "\n" + result.result, result.state)
    })
  }

  private def substituteAtomicStatementHelper(tree: Tree, state: InstrumentState, indent: Int): InstrumentResult = {
    val instrumentation = state.instrumentation
    val cfg = state.cfg
    val getLineNumber = state.getLineNumber
    val spaces = " " * indent

    tree match {
      case _: AssertTree => instrumentation.instrument(tree, state, indent, cfg)
      case tree3: BlockTree =>
        val result = substituteAtomicStatementInSequences(tree3.getStatements.asScala, state, indent + INDENT)
        InstrumentResult(s"$spaces{${result.result}\n$spaces}", result.state)
      case _: BreakTree => instrumentation.instrument(tree, state, indent, cfg)
      case _: ClassTree => errorInstrumentResult(s"Cannot instrument a class tree at line ${getLineNumber(tree)}")
      case _: ContinueTree => instrumentation.instrument(tree, state, indent, cfg)
      case tree3: DoWhileLoopTree =>
        val result = substituteAtomicStatementHelper(tree3.getStatement, state, indent)
        InstrumentResult(
          s"${spaces}do\n${result.result}\n${spaces}while ${tree3.getCondition.toString};",
          result.state)
      case _: EmptyStatementTree => instrumentation.instrument(tree, state, indent, cfg)
      case _: EnhancedForLoopTree => errorInstrumentResult(s"Not yet support enhanced for loop at line ${getLineNumber(tree)}")
      /*s"${spaces}for (${tree.getVariable} : ${tree.getExpression}) {\n" +
        printTreeInstrumentedAtBlock(tree.getStatement, deltaVariable, block, indent + INDENT) +
        s"\n$spaces}"*/
      case _: ExpressionStatementTree => instrumentation.instrument(tree, state, indent, cfg)
      case tree3: ForLoopTree =>
        assert(tree3.getInitializer.size() <= 1)
        assert(tree3.getUpdate.size() <= 1)
        val result1 = substituteAtomicStatementInSequences(tree3.getInitializer.asScala, state, indent + INDENT)
        val result2 = {
          val body = tree3.getStatement match {
            case blockTree: BlockTree =>
              substituteAtomicStatementInSequences(blockTree.getStatements.asScala, result1.state, indent + INDENT + INDENT)
            case _ => errorInstrumentResult("Unreachable")
          }
          val updates = substituteAtomicStatementInSequences(tree3.getUpdate.asScala, body.state, indent + INDENT + INDENT)
          val extraIndent = " " * INDENT
          InstrumentResult(
            s"$spaces${extraIndent}while (${tree3.getCondition}) {" +
              s"${body.result}${updates.result}\n$spaces$extraIndent}"
            , updates.state)
        }
        InstrumentResult(
          s"$spaces{// For loop${result1.result}\n${result2.result}\n$spaces}",
          result2.state
        )
      case tree3: IfTree =>
        val result1 = substituteAtomicStatementHelper(tree3.getThenStatement, state, indent)
        val result2 = substituteAtomicStatementHelper(tree3.getElseStatement, result1.state, indent)
        InstrumentResult(
          s"${spaces}if ${tree3.getCondition}\n${result1.result}\n$spaces" +
            s"else\n${result2.result}",
          result2.state)
      case tree3: LabeledStatementTree =>
        val result = substituteAtomicStatementHelper(tree3.getStatement, state, indent)
        InstrumentResult(s"$spaces${tree3.getLabel.toString}:\n${result.result}", result.state)
      case _: ReturnTree => instrumentation.instrument(tree, state, indent, cfg)
      case _: SwitchTree => errorInstrumentResult(s"Not yet support switch tree at line ${getLineNumber(tree)}")
      // s"${spaces}switch (${tree.getExpression}) {" + s"\n$spaces}"
      case _: SynchronizedTree => errorInstrumentResult(s"Not yet support synchronized tree at line ${getLineNumber(tree)}")
      case _: ThrowTree => errorInstrumentResult(s"Not yet support throw tree at line ${getLineNumber(tree)}")
      case _: TryTree => errorInstrumentResult(s"Not yet support try tree at line ${getLineNumber(tree)}")
      case _: VariableTree => instrumentation.instrument(tree, state, indent, cfg)
      case tree: WhileLoopTree =>
        val result = substituteAtomicStatementHelper(tree.getStatement, state, indent)
        InstrumentResult(s"${spaces}while ${tree.getCondition}\n${result.result}", result.state)
    }
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

  /**
   *
   * @param methodTree    The method whose body will be replaced
   * @param className     The class name of the method
   * @param newMethodBody The new method body
   * @param fileFormat    The file format of the output string
   * @param indent        The indent before the method signature in the output
   * @return A valid Java or C program that contains the new method body
   */
  def replaceMethodBodyAndGenerateSourceCode(methodTree: MethodTree,
                                             className: String,
                                             newMethodBody: String,
                                             fileFormat: FileFormat,
                                             indent: Int): String = {
    val cFilePrefix =
      """extern void __VERIFIER_error() __attribute__((noreturn));
        |extern void __VERIFIER_assume (int);
        |extern int __VERIFIER_nondet_int ();
        |#define static_assert __VERIFIER_assert
        |#define assume __VERIFIER_assume
        |#define LARGE_INT 1000000
        |void __VERIFIER_assert(int cond) {
        |  if (!(cond)) {
        |    ERROR: __VERIFIER_error();
        |  }
        |  return;
        |}
        |void assert(int cond) {
        |  if (!(cond)) {
        |    ERROR: __VERIFIER_error();
        |  }
        |  return;
        |}""".stripMargin

    val methodSignature = {
      // TODO: A very hacky way to get the first line of a method definition
      val lines = methodTree.toString.split("\n")
      // https://stackoverflow.com/a/39259747
      // lines(1).replace(" {", "")
      var firstLine = lines(1)
      firstLine = firstLine.replaceAll("\\n", "")
      firstLine = firstLine.replaceAll("\\r", "")
      assert(firstLine.endsWith(" {"))
      firstLine.substring(0, firstLine.length - 2)
    }
    fileFormat match {
      case JAVA_FORMAT =>
        val spaces = " " * indent
        s"class $className {\n$spaces$methodSignature\n$newMethodBody\n}"
      case C_FORMAT =>
        val replaceMethodSignature = {
          // ICRA requires there exists a method named as `main`
          val startIndex = methodSignature.indexOf(" ")
          val endIndex = methodSignature.indexOf("(")
          s"${methodSignature.substring(0, startIndex + 1)}main${methodSignature.substring(endIndex)}"
        }
        val replaceAssertOne = newMethodBody.replace("assert(true)", "assert(1)")
        s"$cFilePrefix\n$replaceMethodSignature\n$replaceAssertOne"
    }
  }
}
