package brbo.benchmarks

import brbo.common.StringFormatUtils
import brbo.verification.BoundChecking
import org.apache.commons.io.FileUtils
import org.apache.logging.log4j.LogManager

import java.io.File
import java.nio.charset.Charset
import scala.collection.immutable.HashSet

/**
 *
 * @param treeMaxHeight    Max height of the generated AST
 * @param treeMaxWidth     Max width of the generated AST
 * @param resourceVariable Name of the resource variable
 * @param inputs           Names of inputs
 */
class GenerateSyntheticPrograms(treeMaxHeight: Int, treeMaxWidth: Int, resourceVariable: String, inputs: Set[String]) {
  private val random = scala.util.Random
  private val inputsList = inputs.toList

  private def getRandomInput: String = inputsList(random.nextInt(inputs.size))

  private var usedIIndex = 0
  private var usedIteratorIndex = 0
  private var usedEntryIndex = 0

  assert(treeMaxWidth > 0)
  assert(treeMaxHeight > 0)

  def generate: Statement = {
    generateStatement(State(0, None))
  }

  def generateStatement(state: State): Statement = {
    val range = Range.apply(0, random.nextInt(treeMaxWidth))
    val statements: List[BasicStatement] = {
      val list: List[BasicStatement] = range.map({
        _ => generateBasicStatement(state)
      }).toList
      if (list.nonEmpty) list
      else List[BasicStatement](generateBasicStatement(state)) // Avoid generating empty statement
    }
    Statement(statements)
  }

  def generateBasicStatement(state: State): BasicStatement = {
    val choice = random.nextInt(100)
    if (choice < 50 && state.treeHeight < treeMaxHeight) generateLoop(state)
    else generateCommand(state)
  }

  def generateLoop(state: State): Loop = {
    val treeHeight = state.treeHeight
    val lastDefinedEntry = state.lastDefinedEntry
    val choice = random.nextInt(100)
    val upperBound = {
      val choice = random.nextInt(100)
      if (choice < 50 && lastDefinedEntry.isDefined) lastDefinedEntry.get
      else getRandomInput
    }
    if (choice < 50) {
      Loop(
        BasicHeader(getNewI, upperBound),
        generateStatement(State(treeHeight + 1, lastDefinedEntry))
      )
    }
    else {
      val newEntry = getNewEntry
      Loop(
        IteratorHeader(getNewIterator, upperBound, newEntry),
        generateStatement(State(treeHeight + 1, Some(newEntry)))
      )
    }
  }

  def generateCommand(state: State): Command = {
    val choice = random.nextInt(100)
    if (choice < 34) {
      Command(resourceVariable, Symbol(Right(getRandomInput)))
    } else if (choice < 67 && state.lastDefinedEntry.isDefined) {
      Command(resourceVariable, Symbol(Right(state.lastDefinedEntry.get)))
    } else {
      Command(resourceVariable, Symbol(Left(1)))
    }
  }

  case class State(treeHeight: Int, lastDefinedEntry: Option[String])

  private def getNewI: String = {
    val result = s"i$usedIIndex"
    usedIIndex = usedIIndex + 1
    result
  }

  private def getNewIterator: String = {
    val result = s"it$usedIteratorIndex"
    usedIteratorIndex = usedIteratorIndex + 1
    result
  }

  private def getNewEntry: String = {
    val result = s"entry$usedEntryIndex"
    usedEntryIndex = usedEntryIndex + 1
    result
  }
}

object GenerateSyntheticPrograms {
  private val logger = LogManager.getLogger("brbo.benchmarks.GenerateSyntheticPrograms")
  private val MAX_ATTEMPTS = 10000000L
  private val SYNTHETIC_DIRECTORY = s"${System.getProperty("user.dir")}/src/main/java/brbo/benchmarks/synthetic"

  val INDENT: Int = 2

  private def multiplyUpperBound(expression: Expr, entry: String, upperBound: Expr): Expr = {
    expression match {
      case Number(_) => MulExpr(expression, upperBound)
      case Identifier(name) =>
        if (name == entry) upperBound
        else MulExpr(expression, upperBound)
      case AddExpr(left, right) =>
        AddExpr(multiplyUpperBound(left, entry, upperBound), multiplyUpperBound(right, entry, upperBound))
      case MulExpr(_, _) =>
        val baseExpressions = BoundExpression.collectBaseExpr(expression)
        val existsEntry = baseExpressions.exists({
          case Number(_) => false
          case Identifier(name) => name == entry
          case _ => false
        })
        if (existsEntry)
          BoundExpression.substituteBaseExpression(expression, Identifier(entry), upperBound)
        else
          MulExpr(expression, upperBound)
    }
  }

  def generateBound(ast: AST): Expr = {
    val boundExpression = {
      ast match {
        case Statement(statements) =>
          statements.foldLeft(Number(0): Expr)({
            (acc, statement) =>
              val newBound = generateBound(statement)
              if (newBound == Number(0)) acc
              else AddExpr(acc, generateBound(statement))
          })
        case basicStatement: BasicStatement =>
          basicStatement match {
            case Loop(header, statement) =>
              val bodyBound = generateBound(statement)
              header match {
                case BasicHeader(_, upperBound) => MulExpr(Identifier(upperBound), bodyBound)
                case IteratorHeader(_, initialValue, entry) =>
                  val flatten = BoundExpression.flattenToSumOfMulExpr(bodyBound, None)
                  multiplyUpperBound(flatten, entry, Identifier(initialValue))
              }
            case Command(_, update) => generateBound(update)
          }
        case _: Header => throw new Exception("Unexpected")
        case Symbol(value) =>
          value match {
            case Left(integer) => Number(integer)
            case Right(string) => Identifier(string)
          }
      }
    }
    BoundExpression.getRidOfZeroItems(boundExpression)
  }

  def generateSourceCode(numberOfPrograms: Int, treeMaxHeight: Int, treeMaxWidth: Int, resourceVariable: String, inputs: Set[String]): Unit = {
    var set = new HashSet[Statement]
    var i = 0L
    while (set.size < numberOfPrograms && i < MAX_ATTEMPTS) {
      val generateSyntheticPrograms = new GenerateSyntheticPrograms(treeMaxHeight, treeMaxWidth, resourceVariable, inputs)
      val program = generateSyntheticPrograms.generate

      if (!set.contains(program)) {
        val mostPreciseBound = generateBound(program)
        // TODO: Not exactly weakening all coefficients
        val lessPreciseBound = MulExpr(mostPreciseBound, Number(BoundChecking.MAX_COEFFICIENT_VALUE))
        if (mostPreciseBound != Number(0)) {
          val className = StringFormatUtils.integer(set.size, 3)
          val sourceCode =
            s"""package brbo.benchmarks.synthetic;
               |import brbo.benchmarks.Common;
               |public abstract class Synthetic$className extends Common {
               |  void f(${inputs.map(x => s"int $x").mkString(", ")}) {
               |    if (${inputs.map(x => s"$x <= 0").mkString(" || ")}) {
               |      return;
               |    }
               |    int $resourceVariable = 0;
               |    mostPreciseBound($resourceVariable <= $mostPreciseBound);
               |    lessPreciseBound($resourceVariable <= $lessPreciseBound);
               |${program.toString(4)}
               |  }
               |}""".
              stripMargin
          val file = new File(s"$SYNTHETIC_DIRECTORY/Synthetic$className.java")
          FileUtils.writeStringToFile(file, sourceCode, Charset.forName("UTF-8"))
        }
        else {
          logger.error(s"Skip generating programs whose bounds are `${Number(0)}`")
        }
      }

      set = set + program
      i = i + 1
    }
    if (i == MAX_ATTEMPTS) logger.info(s"Cannot generate $numberOfPrograms programs within `$MAX_ATTEMPTS` attempts")
    logger.info(s"Generated ${set.size} programs")
  }
}
