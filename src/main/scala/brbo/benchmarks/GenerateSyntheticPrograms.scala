package brbo.benchmarks

import java.io.File
import java.nio.charset.Charset

import brbo.BrboMain.OUTPUT_DIRECTORY
import org.apache.commons.io.FileUtils
import org.apache.logging.log4j.LogManager

import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future, TimeoutException}

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

  def generate: Statement = {
    generateStatement(State(0, None))
  }

  def generateStatement(state: State): Statement = {
    val statements = Range.apply(0, random.nextInt(treeMaxWidth)).map({
      _ => generateBasicStatement(state)
    }).toList
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

  val INDENT: Int = 2

  def generateSourceCode(numberOfPrograms: Int, treeMaxHeight: Int, treeMaxWidth: Int, resourceVariable: String, inputs: Set[String]): Unit = {
    val future = Future {
      var map = new HashMap[String, Statement]
      while (map.size < numberOfPrograms) {
        val generateSyntheticPrograms = new GenerateSyntheticPrograms(treeMaxHeight, treeMaxWidth, resourceVariable, inputs)
        val program = generateSyntheticPrograms.generate

        val key = program.toString(0)
        if (!map.contains(key)) {
          val sourceCode =
            s"""class Synthetic${map.size} {
               |  void f(${inputs.map(x => s"int $x").mkString(", ")}) {
               |    if (${inputs.map(x => s"$x <= 0").mkString(" || ")})
               |      return;
               |    int $resourceVariable = 0;
               |${program.toString(4)}
               |  }
               |}""".stripMargin
          val file = new File(s"$OUTPUT_DIRECTORY/Synthetic${map.size}.java")
          FileUtils.writeStringToFile(file, sourceCode, Charset.forName("UTF-8"))
        }

        map = map + (key -> program)
      }
    }
    val timeout = 5
    try {
      Await.result(future, Duration(timeout, SECONDS))
    } catch {
      case _: TimeoutException =>
        logger.info(s"Cannot generate $numberOfPrograms programs within `$timeout` seconds. You need to manually kill this process!")
    }
  }
}
