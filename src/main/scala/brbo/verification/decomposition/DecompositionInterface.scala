package brbo.verification.decomposition

import brbo.common.BeforeOrAfterOrThis.BEFORE
import brbo.common.GhostVariableUtils.GhostVariable.{Delta, Resource}
import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.{BrboType, INT}
import brbo.common._
import brbo.common.cfg.CFGUtils
import brbo.common.instrument.FileFormat.JAVA_FORMAT
import brbo.common.instrument.InstrumentUtils.{NewMethodInformation, appendSemiColon}
import brbo.common.instrument.{InstrumentUtils, StatementTreeInstrumentation}
import brbo.verification.AmortizationMode.AmortizationMode
import brbo.verification.CounterAxiomGenerator
import com.sun.source.tree.{ExpressionStatementTree, StatementTree, Tree}
import org.apache.logging.log4j.{LogManager, Logger}
import org.checkerframework.dataflow.cfg.node.Node

import scala.collection.immutable.HashMap

abstract class DecompositionInterface(inputMethod: TargetMethod, arguments: CommandLineArguments) {
  protected val debug: Boolean = arguments.getDebugMode
  protected val logger: Logger = LogManager.getLogger(classOf[Decomposition])

  protected val sortedCommands: List[StatementTree] = inputMethod.sortedCommands

  protected val counterMap: Map[Tree, String] = CounterAxiomGenerator.generateCounterMap(inputMethod.methodTree.getBody)

  protected def traceOrError(message: String): Unit = {
    if (debug) logger.error(message)
    else logger.trace(message)
  }

  def decompose: List[DecompositionResult]

  protected def decompose[T <: Segment](fullAmortize: IntermediateResult[T],
                                        selectiveAmortize: IntermediateResult[T],
                                        noAmortize: IntermediateResult[T]): List[DecompositionResult] = {
    val listOfSubprograms: List[IntermediateResult[T]] = {
      val amortizationMode = arguments.getAmortizationMode
      logger.info(s"Decomposing... Mode: `$amortizationMode`")
      amortizationMode match {
        case brbo.verification.AmortizationMode.NO_AMORTIZE => List(noAmortize)
        case brbo.verification.AmortizationMode.FULL_AMORTIZE => List(fullAmortize)
        case brbo.verification.AmortizationMode.SELECTIVE_AMORTIZE => List(selectiveAmortize)
        case brbo.verification.AmortizationMode.ALL_AMORTIZE =>
          List(noAmortize, selectiveAmortize, fullAmortize)
      }
    }
    listOfSubprograms.map({ result => insertGhostVariables(result) })
  }

  /**
   *
   * @param statement    The statement from which we generate an initial subprogram
   * @param targetMethod The method that encloses the above statement
   * @return The initial program and its entry node in the CFG
   */
  def initializeGroups(statement: StatementTree, targetMethod: TargetMethod): Option[(StatementTree, Node)] = {
    statement match {
      case expressionStatementTree: ExpressionStatementTree =>
        GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource) match {
          case Some(updateTree) =>
            val updateNode = CFGUtils.getNodesForExpressionStatementTree(expressionStatementTree, targetMethod.cfg)

            updateTree.increment match {
              /*case literalTree: LiteralTree =>
                assert(literalTree.getKind == Kind.INT_LITERAL)
                // The initial subprogram is the minimal enclosing loop when `R` is updated by a constant
                val subprogram: StatementTree = {
                  TreeUtils.getMinimalEnclosingLoop(targetMethod.getPath(statement)) match {
                    case Some(enclosingLoop) => enclosingLoop
                    case None =>
                      logger.trace(s"Resource update `$statement` does not have an enclosing loop")
                      statement
                  }
                }.asInstanceOf[StatementTree]
                val entryNode: Node = CFGUtils.entryOfMinimalEnclosingLoop(updateNode, targetMethod) match {
                  case Some(entryNode) => entryNode
                  case None => updateNode
                }
                logger.trace(s"Resource update `$statement`'s initial subprogram is `$subprogram`. Entry node is `$entryNode`")
                Some(subprogram, entryNode)*/
              case _ => Some(statement, updateNode)
            }
          case None => None
        }
      case _ => None
    }
  }

  def insertGhostVariables[T <: Segment](intermediateResult: IntermediateResult[T]): DecompositionResult = {
    val amortizationMode: AmortizationMode = intermediateResult.amortizationMode
    logger.info(s"Inserting resets and updates to ghost variables... Mode: `$amortizationMode`")
    val groups = intermediateResult.groups

    val deltaCounterPairs: Map[T, DeltaCounterPair] = {
      val deltaVariables: Map[T, String] = {
        // Eliminate non-deterministic behavior
        val indexedGroups: List[(T, Int)] = groups.elements.toList.sortWith({
          case (group1, group2) => group1.toString < group2.toString
        }).zipWithIndex
        indexedGroups.foldLeft(new HashMap[T, String])({
          case (acc, (group, index)) =>
            acc + (group -> GhostVariableUtils.generateName(index.toString, Delta))
        })
      }
      groups.elements.foldLeft(new HashMap[T, DeltaCounterPair])({
        (acc, group) =>
          val pair = DeltaCounterPair(deltaVariables(group), counterMap(group.beginCommand))
          acc + (group -> pair)
      })
    }

    val newMethodBody = {
      val ghostVariableDeclaration = {
        val ghostVariables = deltaCounterPairs.values.foldLeft(new HashMap[String, BrboType])({
          (acc, pair) => acc + (pair.delta -> INT) + (GhostVariableUtils.generateDeltaVariablePrime(pair.delta) -> INT) + (pair.counter -> INT)
        })
        val spaces = " " * 4
        val declarations = ghostVariables
          .map(pair => BrboType.variableDeclarationAndInitialization(pair._1, pair._2, JAVA_FORMAT))
          .toList.sortWith(_ < _).mkString(s"\n$spaces")
        s"$spaces$declarations"
      }
      val newMethodBody = InstrumentUtils.instrumentStatementTrees(
        inputMethod,
        StatementTreeInstrumentation(
          Locations(shouldInstrument(groups), BEFORE),
          whatToInsert(deltaCounterPairs)
        ),
        indent = 2
      )
      // TODO: A very hacky way to insert variable declarations
      newMethodBody.replaceFirst("""\{""", s"{\n$ghostVariableDeclaration")
    }
    val newSourceFile = InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(
      inputMethod,
      NewMethodInformation(None, None, None, List("import brbo.benchmarks.Common;"), Some("Common"), isAbstractClass = true, newMethodBody),
      JAVA_FORMAT,
      indent = 2
    )
    val result = DecompositionResult(newSourceFile, deltaCounterPairs.values.toSet, amortizationMode, inputMethod)
    if (debug || arguments.getPrintCFG) CFGUtils.printPDF(result.outputMethod.cfg, Some("decomposed-"))
    result
  }

  private def shouldInstrument[T <: Segment](groups: Groups[T])(tree: StatementTree): Boolean = {
    if (tree == null) return false

    val insertResetsAndCounterUpdates = groups.elements.exists({
      group => group.beginCommand == tree
    })

    val insertUpdates = {
      if (TreeUtils.isCommand(tree)) {
        tree match {
          case expressionStatementTree: ExpressionStatementTree =>
            GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource).isDefined
          case _ => false
        }
      }
      else false
    }

    insertResetsAndCounterUpdates || insertUpdates
  }

  private def whatToInsert[T <: Segment](deltaCounterPairs: Map[T, DeltaCounterPair])(tree: StatementTree): String = {
    if (tree == null || !TreeUtils.isCommand(tree)) return ""

    val groups = deltaCounterPairs.keySet
    val resets: String = {
      val groupsBeginningWithCommand = groups.filter({
        group => group.beginCommand == tree
      })
      groupsBeginningWithCommand.map({
        group =>
          val pair = deltaCounterPairs(group)
          val deltaPrime = GhostVariableUtils.generateDeltaVariablePrime(pair.delta)
          // s"$deltaPrime = ($deltaPrime > ${pair.delta}) ? $deltaPrime: ${pair.delta}; ${pair.delta} = 0; ${pair.counter} = ${pair.counter} + 1;"
          s"$deltaPrime = ${pair.delta}; ${pair.delta} = 0; ${pair.counter} = ${pair.counter} + 1;"
      }).mkString(" ")
    }

    val updates: String = {
      tree match {
        case expressionStatementTree: ExpressionStatementTree =>
          GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource) match {
            case Some(updateTree) =>
              val groupsContainingCommand = groups.filter(group => group.containCommand(tree))
              groupsContainingCommand.size match {
                case 0 => ""
                case 1 =>
                  val pair = deltaCounterPairs(groupsContainingCommand.head)
                  s"${pair.delta} = ${pair.delta} + ${updateTree.increment}"
                case 2 => throw new Exception(s"Command `$tree` must only belong to one group!")
              }
            case None => ""
          }
        case _ => ""
      }
    }

    s"${appendSemiColon(resets)}${appendSemiColon(updates)}"
  }
}