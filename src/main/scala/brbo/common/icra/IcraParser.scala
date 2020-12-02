package brbo.common.icra

import brbo.common.icra.IcraLexer.{IDENTIFIER, NUMBER}
import org.apache.logging.log4j.LogManager

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

class IcraParser(input: String) {
  private val logger = LogManager.getLogger(classOf[IcraParser])
  private val prefix = "intraproceduralWeight = Base relation: {"
  private val suffix = "*******************************************"

  case class Invariant(declarations: String, invariant: String)

  def parseInvariant(invariant: Invariant): Unit = {

  }

  def getRawInvariants: List[Invariant] = {
    def parseInvariant(invariant: String): Invariant = {
      logger.trace(s"Parsing invariant: $invariant")
      val keywordWhen = " when "
      val indexOfWhen = invariant.indexOf(keywordWhen)
      val part1 = invariant.substring(0, indexOfWhen)
      val part2 = invariant.substring(indexOfWhen + keywordWhen.length)
      logger.trace(s"Declarations: $part1; Invariant$part2")
      Invariant(part1, part2)
    }

    var rawInvariants: List[Invariant] = Nil
    var prefixSearchIndex = 0
    var suffixSearchIndex = 0
    var prefixIndex = 0
    var suffixIndex = 0
    var keepGoing = true
    while (keepGoing) {
      prefixIndex = input.indexOf(prefix, prefixSearchIndex)
      suffixIndex = input.indexOf(suffix, suffixSearchIndex)
      if (prefixIndex != -1 && suffixIndex != -1) {
        assert(prefixIndex < suffixIndex)
        logger.trace(s"Index of prefix: $prefixIndex; Index of suffix: $suffixIndex")
        val invariant = {
          val invariant = input.substring(prefixIndex + prefix.length, suffixIndex - 1)
          logger.trace(s"Collected invariant: $invariant")
          parseInvariant(invariant)
        }
        rawInvariants = invariant :: rawInvariants
        prefixSearchIndex = suffixIndex + suffix.length
        suffixSearchIndex = prefixSearchIndex
      }
      else {
        keepGoing = false
      }
    }
    rawInvariants.reverse
  }
}

object WorkflowParser extends Parsers {
  override type Elem = IcraLexer.IcraToken

  sealed trait IcraAST

  case class Identifier(identifier: String) extends IcraAST

  case class Number(number: Int) extends IcraAST

  case class Bracket(ast: IcraAST) extends IcraAST

  case class Addition(left: IcraAST, right: IcraAST) extends IcraAST

  case class Subtraction(left: IcraAST, right: IcraAST) extends IcraAST

  case class Multiplication(left: IcraAST, right: IcraAST) extends IcraAST

  case class LessThan(left: IcraAST, right: IcraAST) extends IcraAST

  case class LessThanOrEqualTo(left: IcraAST, right: IcraAST) extends IcraAST

  case class GreaterThan(left: IcraAST, right: IcraAST) extends IcraAST

  case class GreaterThanOrEqualTo(left: IcraAST, right: IcraAST) extends IcraAST

  case class Equal(left: IcraAST, right: IcraAST) extends IcraAST

  case class And(left: IcraAST, right: IcraAST) extends IcraAST

  case class Or(left: IcraAST, right: IcraAST) extends IcraAST

  case class Negation(ast: IcraAST) extends IcraAST

  private def identifier: Parser[IDENTIFIER] = {
    accept("identifier", { case identifier@IDENTIFIER(_) => identifier })
  }

  private def number: Parser[NUMBER] = {
    accept("number", { case number@NUMBER(_) => number })
  }

  class WorkflowTokenReader(tokens: Seq[IcraLexer.IcraToken]) extends Reader[IcraLexer.IcraToken] {
    override def first: IcraLexer.IcraToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[IcraLexer.IcraToken] = new WorkflowTokenReader(tokens.tail)
  }

  def program: Parser[List[IcraAST]] = {
    phrase(block)
  }

  def block: Parser[List[IcraAST]] = {
    rep1(declaration) ^^ { case declarations => declarations }
  }

  def declaration: Parser[IcraAST] = {
    ???
  }
}