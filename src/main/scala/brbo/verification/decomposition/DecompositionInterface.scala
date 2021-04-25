package brbo.verification.decomposition

import brbo.common.BeforeOrAfterOrThis.BEFORE
import brbo.common.GhostVariableUtils.GhostVariable.{Delta, Resource}
import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.{BrboType, INT}
import brbo.common._
import brbo.common.instrument.FileFormat.JAVA_FORMAT
import brbo.common.instrument.InstrumentUtils.{NewMethodInformation, appendSemiColon}
import brbo.common.instrument.{InstrumentUtils, StatementTreeInstrumentation}
import brbo.verification.AmortizationMode.AmortizationMode
import brbo.verification.CounterAxiomGenerator
import com.sun.source.tree.{ExpressionStatementTree, StatementTree, Tree}
import org.apache.logging.log4j.{LogManager, Logger}

import scala.collection.immutable.HashMap

abstract class DecompositionInterface(inputMethod: TargetMethod, arguments: CommandLineArguments) {
  protected val debug: Boolean = arguments.getDebugMode
  protected val logger: Logger = LogManager.getLogger(classOf[Decomposition])

  protected val commands: List[StatementTree] = inputMethod.commands

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
    if (tree == null) return ""

    val groups = deltaCounterPairs.keySet
    val prepend1: String = groups.find({
      group => group.beginCommand == tree
    }) match {
      case Some(group) =>
        val pair = deltaCounterPairs(group)
        val deltaPrime = GhostVariableUtils.generateDeltaVariablePrime(pair.delta)
        // s"$deltaPrime = ($deltaPrime > ${pair.delta}) ? $deltaPrime: ${pair.delta}; ${pair.delta} = 0; ${pair.counter} = ${pair.counter} + 1;"
        s"$deltaPrime = ${pair.delta}; ${pair.delta} = 0; ${pair.counter} = ${pair.counter} + 1;"
      case None => ""
    }

    val prepend2: String = {
      if (TreeUtils.isCommand(tree)) {
        tree match {
          case expressionStatementTree: ExpressionStatementTree =>
            GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource) match {
              case Some(updateTree) =>
                // Assume there is only 1 subprogram that contains command `R=R+e`
                groups.find(group => group.containCommand(tree)) match {
                  case Some(subprogram) =>
                    val pair = deltaCounterPairs(subprogram)
                    s"${pair.delta} = ${pair.delta} + ${updateTree.increment}"
                  case None => ""
                }
              case None => ""
            }
          case _ => ""
        }
      }
      else ""
    }

    s"${appendSemiColon(prepend1)}${appendSemiColon(prepend2)}"
  }
}