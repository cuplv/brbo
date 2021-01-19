package brbo.common

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.InstrumentUtils.FileFormat.{C_FORMAT, FileFormat, JAVA_FORMAT}
import brbo.common.InstrumentUtils.InstrumentMode.{ALL, AT_MOST_ONCE, InstrumentMode}
import brbo.common.TypeUtils.BrboType
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.node.Node

import scala.collection.JavaConverters._

object InstrumentUtils {
  private val logger = LogManager.getLogger("brbo.common.InstrumentUtils")

  val INDENT = 2

  val defaultDeltaVariable = "D100"
  val defaultResourceAssignment: AtomicStatementInstrumentation =
    AtomicStatementInstrumentation( // Replace `R = R + e` with `d = d + e`
      {
        node: Node => GhostVariableUtils.extractGhostVariableUpdate(node, Resource).nonEmpty
      },
      {
        case expressionStatement: ExpressionStatementTree =>
          expressionStatement.getExpression match {
            case assignmentTree: AssignmentTree =>
              val update = GhostVariableUtils.extractGhostVariableUpdate(assignmentTree, Resource)
              s"$defaultDeltaVariable = $defaultDeltaVariable + ${update.get.increment}"
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
        node: Node => GhostVariableUtils.extractGhostVariableUpdate(node, Resource).nonEmpty
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

  @deprecated
  def substituteAtomicStatements(targetMethod: TargetMethod,
                                 instrumentation: AtomicStatementInstrumentation,
                                 indent: Int,
                                 instrumentMode: InstrumentMode): InstrumentResult = {
    val tree = targetMethod.methodTree.getBody
    val cfg: ControlFlowGraph = targetMethod.cfg
    val getLineNumber: Tree => Int = targetMethod.getLineNumber

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
  @deprecated
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
  @deprecated
  case class InstrumentResult(result: String, state: InstrumentState) {
    assert(!result.contains("InstrumentResult"))
  }

  @deprecated
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

  @deprecated
  private def errorInstrumentResult(errorMessage: String): InstrumentResult = throw new RuntimeException(errorMessage)

  @deprecated
  private def substituteAtomicStatementInSequences(statementTrees: Iterable[Tree], state: InstrumentState, indent: Int): InstrumentResult = {
    statementTrees.foldLeft(InstrumentResult("", state))({
      (acc, statementTree) =>
        val result = substituteAtomicStatementHelper(statementTree, acc.state, indent)
        InstrumentResult(acc.result + "\n" + result.result, result.state)
    })
  }

  @deprecated
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
        /*val result = substituteAtomicStatementHelper(tree3.getStatement, state, indent)
        InstrumentResult(
          s"${spaces}do\n${result.result}\n${spaces}while ${tree3.getCondition.toString};",
          result.state)*/
        errorInstrumentResult(s"Not yet support do while loop at line ${getLineNumber(tree)}")
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

  /**
   *
   * @param targetMethod  The method whose body will be replaced
   * @param newParameters The new method parameters
   * @param newMethodBody The new method body
   * @param fileFormat    The file format of the output string
   * @param indent        The indent before the method signature in the output
   * @return A valid Java or C program that contains the new method body
   */
  def replaceMethodBodyAndGenerateSourceCode(targetMethod: TargetMethod,
                                             newParameters: Option[String],
                                             newClassName: Option[String],
                                             packageName: Option[String],
                                             imports: List[String],
                                             extendsClass: Option[String],
                                             isAbstractClass: Boolean,
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
        |#define true 1
        |#define false 0
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
        |}
        |int ndInt() {
        |  return __VERIFIER_nondet_int();
        |}
        |int ndBool() {
        |  int x = ndInt();
        |  assume(x == 1 || x == 0);
        |  return x;
        |}
        |int ndInt2(int lower, int upper) {
        |  int x = ndInt();
        |  assume(lower <= x && x <= upper);
        |  return x;
        |}
        |""".stripMargin

    val methodSignature: String = {
      val parameters = newParameters match {
        case Some(parameters) => parameters
        case None => targetMethod.inputVariables.map(pair => BrboType.variableDeclaration(pair._1, pair._2)).mkString(", ")
      }
      s"${BrboType.toString(targetMethod.returnType, fileFormat)} ${targetMethod.methodTree.getName.toString}($parameters)"
    }
    fileFormat match {
      case JAVA_FORMAT =>
        val spaces = " " * indent
        val className = newClassName match {
          case Some(value) => value
          case None => targetMethod.className
        }
        val importsString = imports.mkString("\n")
        val packageNameString = packageName match {
          case Some(value) => s"package $value;\n"
          case None => ""
        }
        val extendsClassString = extendsClass match {
          case Some(value) => s"extends $value"
          case None => ""
        }
        val abstractString = if (isAbstractClass) "abstract " else ""
        s"$packageNameString$importsString\n${abstractString}class $className $extendsClassString {\n" +
          s"$spaces$methodSignature\n$newMethodBody\n}"
      case C_FORMAT =>
        val replaceMethodSignature = {
          // ICRA requires there exists a method named as `main`
          val startIndex = methodSignature.indexOf(" ")
          val endIndex = methodSignature.indexOf("(")
          s"${methodSignature.substring(0, startIndex + 1)}main${methodSignature.substring(endIndex)}"
        }
        // val replaceAssertOne = newMethodBody.replace("assert(true)", "assert(1)")
        s"$cFilePrefix\n$replaceMethodSignature\n$newMethodBody"
    }
  }

  object InstrumentMode extends Enumeration {
    type InstrumentMode = Value
    val AT_MOST_ONCE, ALL = Value
  }

  object FileFormat extends Enumeration {
    type FileFormat = Value
    val JAVA_FORMAT, C_FORMAT = Value
  }

  case class StatementTreeInstrumentation(locations: Locations, whatToInsert: StatementTree => String)

  def instrumentStatementTrees(targetMethod: TargetMethod, instrumentation: StatementTreeInstrumentation, indent: Int): String = {
    instrumentStatementTreesHelper(targetMethod.methodTree.getBody, instrumentation, indent)
  }

  private def instrumentStatementTreesHelper(tree: StatementTree, instrumentation: StatementTreeInstrumentation, indent: Int): String = {
    if (tree == null) return ""

    val spaces = " " * indent
    val part1: String = tree match {
      case _ if TreeUtils.isCommand(tree) => s"$spaces${tree.toString};"
      case blockTree: BlockTree =>
        val body = instrumentStatementTreesHelper2(blockTree.getStatements.asScala, instrumentation, indent + INDENT)
        s"$spaces{\n$body\n$spaces}"
      case forLoopTree: ForLoopTree =>
        assert(forLoopTree.getInitializer.size() <= 1)
        assert(forLoopTree.getUpdate.size() <= 1)
        val result1 = instrumentStatementTreesHelper2(forLoopTree.getInitializer.asScala, instrumentation, indent + INDENT)
        val result2 = {
          val body = forLoopTree.getStatement match {
            case blockTree: BlockTree =>
              instrumentStatementTreesHelper2(blockTree.getStatements.asScala, instrumentation, indent + INDENT + INDENT)
            case _ => throw new Exception("Unreachable")
          }
          val updates = instrumentStatementTreesHelper2(forLoopTree.getUpdate.asScala, instrumentation, indent + INDENT + INDENT)
          val extraIndent = " " * INDENT
          s"$spaces${extraIndent}while (${forLoopTree.getCondition}) {\n" +
            s"$body\n$updates\n$spaces$extraIndent}"
        }
        s"$spaces{// For loop\n$result1\n$result2\n$spaces}"
      case ifTree: IfTree =>
        val result1 = instrumentStatementTreesHelper(ifTree.getThenStatement, instrumentation, indent)
        val result2 = {
          val result2 = instrumentStatementTreesHelper(ifTree.getElseStatement, instrumentation, indent)
          if (result2 == "") ""
          else s"else\n$result2"
        }
        s"${spaces}if ${ifTree.getCondition}\n$result1\n$spaces$result2"
      case labeledStatementTree: LabeledStatementTree =>
        val result = instrumentStatementTreesHelper(labeledStatementTree.getStatement, instrumentation, indent)
        s"$spaces${labeledStatementTree.getLabel.toString}:\n$result"
      case whileLoopTree: WhileLoopTree =>
        val result = instrumentStatementTreesHelper(whileLoopTree.getStatement, instrumentation, indent)
        s"${spaces}while ${whileLoopTree.getCondition}\n$result"
      case _ => throw new Exception(s"Unsupported tree: `$tree`")
    }

    if (instrumentation.locations.predicate(tree)) {
      val part2 = s"$spaces${instrumentation.whatToInsert(tree)}"
      instrumentation.locations.beforeOrAfter match {
        case brbo.common.BeforeOrAfter.BEFORE => s"$part2\n$part1"
        case brbo.common.BeforeOrAfter.AFTER => s"$part1\n$part2"
      }
    }
    else part1
  }

  private def instrumentStatementTreesHelper2(trees: Iterable[StatementTree], instrumentation: StatementTreeInstrumentation, indent: Int): String = {
    trees.map(tree => instrumentStatementTreesHelper(tree, instrumentation, indent)).mkString("\n")
  }
}
