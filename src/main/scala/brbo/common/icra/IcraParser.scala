package brbo.common.icra

import brbo.common.icra.IcraLexer._
import org.apache.logging.log4j.LogManager

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

class IcraParser(input: String) {
  private val logger = LogManager.getLogger(classOf[IcraParser])
  private val prefix = "intraproceduralWeight = Base relation: {"
  private val suffix = "*******************************************"

  case class Invariant(declarations: String, invariant: String)

  def parseInvariant(invariant: Invariant): IcraAST = {
    ???
  }

  def extractRawInvariants: List[Invariant] = {
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

sealed trait IcraAST

case class Identifier(identifier: String) extends IcraAST

case class Number(number: Int) extends IcraAST

case class Bracket(expression: IcraAST) extends IcraAST

case class Addition(left: IcraAST, right: IcraAST) extends IcraAST

case class Subtraction(left: IcraAST, right: IcraAST) extends IcraAST

case class Multiplication(left: IcraAST, right: IcraAST) extends IcraAST

case class Division(left: IcraAST, right: IcraAST) extends IcraAST

case class Negative(expression: IcraAST) extends IcraAST

case class LessThan(left: IcraAST, right: IcraAST) extends IcraAST

case class LessThanOrEqualTo(left: IcraAST, right: IcraAST) extends IcraAST

case class GreaterThan(left: IcraAST, right: IcraAST) extends IcraAST

case class GreaterThanOrEqualTo(left: IcraAST, right: IcraAST) extends IcraAST

case class Equal(left: IcraAST, right: IcraAST) extends IcraAST

case class And(left: IcraAST, right: IcraAST) extends IcraAST

case class Or(left: IcraAST, right: IcraAST) extends IcraAST

case class Negation(expression: IcraAST) extends IcraAST

case class Assignment(variable: Identifier, expression: IcraAST) extends IcraAST

object IcraParser extends Parsers {
  override type Elem = IcraLexer.IcraToken

  class IcraTokenReader(tokens: Seq[IcraLexer.IcraToken]) extends Reader[IcraLexer.IcraToken] {
    override def first: IcraLexer.IcraToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[IcraLexer.IcraToken] = new IcraTokenReader(tokens.tail)
  }

  def parseInvariant(input: String): IcraAST = {
    val tokens: Seq[IcraToken] = IcraLexer.parse(input)
    val reader = new IcraTokenReader(tokens)
    invariant(reader) match {
      case NoSuccess(message, next) => throw new RuntimeException(s"Error when parsing invariant tokens `$tokens`: $message; Next: $next")
      case Success(result, next) => result
    }
  }

  def parseDeclarations(input: String): List[IcraAST] = {
    val tokens: Seq[IcraToken] = IcraLexer.parse(input)
    val reader = new IcraTokenReader(tokens)
    declarations(reader) match {
      case NoSuccess(message, next) => throw new RuntimeException(s"Error when parsing declaration tokens `$tokens`: $message; Next: $next")
      case Success(result, next) => result
    }
  }

  def parseBoolExpression(input: String): IcraAST = {
    val tokens: Seq[IcraToken] = IcraLexer.parse(input)
    val reader = new IcraTokenReader(tokens)
    phrase(boolExpression)(reader) match {
      case NoSuccess(message, next) => throw new RuntimeException(s"Error when parsing declaration tokens `$tokens`: $message; Next: $next")
      case Success(result, next) => result
    }
  }

  def parseArithmeticExpression(input: String): IcraAST = {
    val tokens: Seq[IcraToken] = IcraLexer.parse(input)
    val reader = new IcraTokenReader(tokens)
    phrase(expression)(reader) match {
      case NoSuccess(message, next) => throw new RuntimeException(s"Error when parsing declaration tokens `$tokens`: $message; Next: $next")
      case Success(result, next) => result
    }
  }

  private def invariant: Parser[IcraAST] = {
    phrase(boolExpression)
  }

  private def declarations: Parser[List[IcraAST]] = {
    phrase(rep1(declaration)) ^^ (declarations => declarations)
  }

  private def declaration: Parser[Assignment] = {
    (identifier ~ ASSIGNMENT ~ expression) ^^ {
      case identifier ~ _ ~ expression => Assignment(identifier, expression)
    }
  }

  private def boolExpression: Parser[IcraAST] = {
    boolTerm ^^ {
      case boolTerm: IcraAST => boolTerm
    }
  }

  private def boolTerm: Parser[IcraAST] = {
    (boolFactor | boolTerm ~ OR ~ boolFactor) ^^ {
      case (left: IcraAST) ~ OR ~ (right: IcraAST) => Or(left, right)
      case boolFactor: IcraAST => boolFactor
    }
  }

  private def boolFactor: Parser[IcraAST] = {
    (boolSecondary |
      boolFactor ~ AND ~ boolSecondary) ^^ {
      case (left: IcraAST) ~ AND ~ (right: IcraAST) => And(left, right)
      case boolSecondary: IcraAST => boolSecondary
    }
  }

  private def boolSecondary: Parser[IcraAST] = {
    (boolPrimary | NEGATION ~ boolPrimary) ^^ {
      case NEGATION ~ (boolPrimary: IcraAST) => Negation(boolPrimary)
      case boolPrimary: IcraAST => boolPrimary
    }
  }

  private def boolPrimary: Parser[IcraAST] = {
    (identifier |
      // TODO: Boolean literals
      relational |
      LEFT_BRACKET ~ boolExpression ~ RIGHT_BRACKET) ^^ {
      case identifier: Identifier => identifier
      case relational: IcraAST => relational
      case LEFT_BRACKET ~ (boolExpression: IcraAST) ~ RIGHT_BRACKET => boolExpression
    }
  }

  private def relational: Parser[IcraAST] = {
    (expression ~ EQUAL ~ expression |
      expression ~ LESS_THAN ~ expression |
      expression ~ LESS_THAN_OR_EQUAL_TO ~ expression |
      expression ~ GREATER_THAN ~ expression |
      expression ~ GREATER_THAN_OR_EQUAL_TO ~ expression) ^^ {
      case (left: IcraAST) ~ EQUAL ~ (right: IcraAST) => Equal(left, right)
      case (left: IcraAST) ~ LESS_THAN ~ (right: IcraAST) => LessThan(left, right)
      case (left: IcraAST) ~ LESS_THAN_OR_EQUAL_TO ~ (right: IcraAST) => LessThanOrEqualTo(left, right)
      case (left: IcraAST) ~ GREATER_THAN ~ (right: IcraAST) => GreaterThan(left, right)
      case (left: IcraAST) ~ GREATER_THAN_OR_EQUAL_TO ~ (right: IcraAST) => GreaterThanOrEqualTo(left, right)
    }
  }

  private def expression: Parser[IcraAST] = {
    (term |
      PLUS ~ term |
      MINUS ~ term |
      expression ~ PLUS ~ term |
      expression ~ MINUS ~ term) ^^ {
      case (left: IcraAST) ~ PLUS ~ (right: IcraAST) => Addition(left, right)
      case (left: IcraAST) ~ MINUS ~ (right: IcraAST) => Subtraction(left, right)
      case PLUS ~ (term: IcraAST) => term
      case MINUS ~ (term: IcraAST) => Negative(term)
      case term: IcraAST => term
    }
  }

  private def term: Parser[IcraAST] = {
    (factor |
      term ~ MULTIPLICATION ~ factor |
      term ~ DIVISION ~ factor) ^^ {
      case (term: IcraAST) ~ MULTIPLICATION ~ (factor: IcraAST) => Multiplication(term, factor)
      case (term: IcraAST) ~ DIVISION ~ (factor: IcraAST) => Division(term, factor)
      case factor: IcraAST => factor
    }
  }

  private def factor: Parser[IcraAST] = {
    primary ^^ {
      primary: IcraAST => primary
    }
  }

  private def primary: Parser[IcraAST] = {
    (number | identifier | LEFT_BRACKET ~ expression ~ RIGHT_BRACKET) ^^ {
      case LEFT_BRACKET ~ (expression: IcraAST) ~ RIGHT_BRACKET => expression
      case number: Number => number
      case identifier: Identifier => identifier
    }
  }

  private def identifier: Parser[Identifier] = {
    accept("identifier", { case IDENTIFIER(identifier) => Identifier(identifier) })
  }

  private def number: Parser[Number] = {
    accept("number", { case NUMBER(number) => Number(number) })
  }
}

// Bool and arithmetic expression: https://compilers.iecc.com/crenshaw/tutor6.txt
// Bool expression: http://www.cs.unb.ca/~wdu/cs4613/a2ans.htm
// Arithmetic expression: https://stackoverflow.com/a/34603099
// Other helpful answers:
// https://stackoverflow.com/a/2976708
// https://stackoverflow.com/a/43971433