package brbo.common.icra

import brbo.common.icra.IcraLexer._
import org.apache.logging.log4j.LogManager

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

class IcraParser(input: String) {
  private val logger = LogManager.getLogger(classOf[IcraParser])
  private val prefix = "intraproceduralWeight = Base relation: {"
  private val suffix = "*******************************************"
  private val assertionChecking = "Assertion Checking at Error Points"

  if (!input.contains(assertionChecking)) {
    logger.fatal(s"No assertion is checked by ICRA! ICRA's output is: $input")
  }

  def parseRawInvariant(rawInvariant: RawInvariant): ParsedInvariant = {
    ParsedInvariant(
      IcraParser.parseDeclarations(rawInvariant.declarations),
      IcraParser.parseInvariant(rawInvariant.invariant)
    )
  }

  def extractRawInvariants: List[RawInvariant] = {
    def parseInvariant(invariant: String): RawInvariant = {
      logger.trace(s"Parsing invariant: $invariant")
      val keywordWhen = " when "
      val indexOfWhen = {
        val indexOfWhen = invariant.indexOf(keywordWhen)
        if (indexOfWhen == -1) {
          logger.debug(s"No invariant is inferred: $invariant")
          invariant.length
        }
        else indexOfWhen
      }

      val part1 = invariant.substring(0, indexOfWhen)
      val part2 = {
        if (indexOfWhen == invariant.length) "0 = 0" // Denote a unuseful invariant
        else invariant.substring(indexOfWhen + keywordWhen.length)
      }
      logger.trace(s"Declarations: $part1; Invariant$part2")
      RawInvariant(part1, part2)
    }

    var rawInvariants: List[RawInvariant] = Nil
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

case class RawInvariant(declarations: String, invariant: String)

case class ParsedInvariant(declarations: List[IcraAST], invariant: IcraAST)

sealed trait IcraAST

object EmptyAST extends IcraAST // Intermediate AST

case class Identifier(identifier: String) extends IcraAST

// case class BoolIdentifier(identifier: String) extends IcraAST

case class Number(number: Int) extends IcraAST

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

case class ToAdd(right: IcraAST) extends IcraAST // Intermediate AST

case class ToSubtract(right: IcraAST) extends IcraAST // Intermediate AST

case class ToMultiply(right: IcraAST) extends IcraAST // Intermediate AST

case class ToDivide(right: IcraAST) extends IcraAST // Intermediate AST

case class ToAnd(right: IcraAST) extends IcraAST // Intermediate AST

case class ToOr(right: IcraAST) extends IcraAST // Intermediate AST

case class Negation(expression: IcraAST) extends IcraAST

case class IfThenElse(condition: IcraAST, left: IcraAST, right: IcraAST) extends IcraAST

case class Assignment(variable: Identifier, expression: IcraAST) extends IcraAST // Declare symbols that are used in the inferred invariants

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
      boolTerm: IcraAST => boolTerm
    }
  }

  private def boolTerm: Parser[IcraAST] = {
    (boolFactor ~ boolTermPrime) ^^ {
      case (boolFactor: IcraAST) ~ (boolTermPrime: IcraAST) =>
        boolTermPrime match {
          case EmptyAST => boolFactor
          case ToOr(right) => Or(boolFactor, right)
          case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
        }
    }
  }

  private def boolTermPrime: Parser[IcraAST] = {
    opt(OR ~ boolFactor ~ boolTermPrime) ^^ {
      case Some(OR ~ (boolFactor: IcraAST) ~ (boolTermPrime: IcraAST)) =>
        val thisFactor =
          boolTermPrime match {
            case EmptyAST => boolFactor
            case ToOr(right) => Or(boolFactor, right)
            case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
          }
        ToOr(thisFactor)
      case None => EmptyAST
      case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
    }
  }

  private def boolFactor: Parser[IcraAST] = {
    (boolSecondary ~ boolFactorPrime) ^^ {
      case (boolSecondary: IcraAST) ~ (boolFactorPrime: IcraAST) =>
        boolFactorPrime match {
          case EmptyAST => boolSecondary
          case ToAnd(right) => And(boolSecondary, right)
          case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
        }
    }
  }

  private def boolFactorPrime: Parser[IcraAST] = {
    opt(AND ~ boolSecondary ~ boolFactorPrime) ^^ {
      case Some(AND ~ (boolSecondary: IcraAST) ~ (boolFactorPrime: IcraAST)) =>
        val thisFactor =
          boolFactorPrime match {
            case EmptyAST => boolSecondary
            case ToAnd(right) => And(boolSecondary, right)
            case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
          }
        ToAnd(thisFactor)
      case None => EmptyAST
      case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
    }
  }

  private def boolSecondary: Parser[IcraAST] = {
    (boolPrimary | NEGATION ~ boolPrimary) ^^ {
      case NEGATION ~ (boolPrimary: IcraAST) => Negation(boolPrimary)
      case boolPrimary: IcraAST => boolPrimary
    }
  }

  private def boolPrimary: Parser[IcraAST] = {
    (relational | // TODO: Boolean literals
      LEFT_BRACKET ~ boolExpression ~ RIGHT_BRACKET |
      expression) ^^ {
      // case identifier: Identifier => identifier // TODO: Do we need this???
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
    term ~ expressionPrime ^^ {
      case (term: IcraAST) ~ (expressionPrime: IcraAST) =>
        expressionPrime match {
          case EmptyAST => term
          case ToAdd(right) => Addition(term, right)
          case ToSubtract(right) => Subtraction(term, right)
          case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
        }
      case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
    }
  }

  private def expressionPrime: Parser[IcraAST] = {
    opt(PLUS ~ term ~ expressionPrime | MINUS ~ term ~ expressionPrime) ^^ {
      case Some(PLUS ~ (term: IcraAST) ~ (expressionPrime: IcraAST)) =>
        val thisTerm =
          expressionPrime match {
            case EmptyAST => term
            case ToAdd(right) => Addition(term, right)
            case ToSubtract(right) => Subtraction(term, right)
            case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
          }
        ToAdd(thisTerm)
      case Some(MINUS ~ (term: IcraAST) ~ (expressionPrime: IcraAST)) =>
        val thisTerm =
          expressionPrime match {
            case EmptyAST => term
            case ToAdd(right) => Addition(term, right)
            case ToSubtract(right) => Subtraction(term, right)
            case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
          }
        ToSubtract(thisTerm)
      case None => EmptyAST
      case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
    }
  }

  private def term: Parser[IcraAST] = {
    (factor ~ termPrime) ^^ {
      case (factor: IcraAST) ~ (termPrime: IcraAST) =>
        termPrime match {
          case EmptyAST => factor
          case ToMultiply(right) => Multiplication(factor, right)
          case ToDivide(right) => Division(factor, right)
          case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
        }
      case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
    }
  }

  private def termPrime: Parser[IcraAST] = {
    opt(MULTIPLICATION ~ factor ~ termPrime | DIVISION ~ factor ~ termPrime) ^^ {
      case Some(MULTIPLICATION ~ (factor: IcraAST) ~ (termPrime: IcraAST)) =>
        val thisFactor =
          termPrime match {
            case EmptyAST => factor
            case ToMultiply(right) => Multiplication(factor, right)
            case ToDivide(right) => Division(factor, right)
            case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
          }
        ToMultiply(thisFactor)
      case Some(DIVISION ~ (factor: IcraAST) ~ (termPrime: IcraAST)) =>
        val thisFactor =
          termPrime match {
            case EmptyAST => factor
            case ToMultiply(right) => Multiplication(factor, right)
            case ToDivide(right) => Division(factor, right)
            case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
          }
        ToDivide(thisFactor)
      case None => EmptyAST
      case x@_ => throw new RuntimeException(s"$x is unexpected during parsing")
    }
  }

  private def factor: Parser[IcraAST] = {
    (primary | PLUS ~ factor | MINUS ~ factor) ^^ {
      case PLUS ~ (factor: IcraAST) => factor
      case MINUS ~ (factor: IcraAST) => Negative(factor)
      case primary: IcraAST => primary
    }
  }

  private def primary: Parser[IcraAST] = {
    (identifier ~ LEFT_BRACKET ~ boolExpression ~ COMMA ~ expression ~ COMMA ~ expression ~ RIGHT_BRACKET |
      number |
      identifier |
      LEFT_BRACKET ~ expression ~ RIGHT_BRACKET) ^^ {
      case (identifier: Identifier) ~ _ ~ (condition: IcraAST) ~ _ ~ (left: IcraAST) ~ _ ~ (right: IcraAST) ~ _ =>
        assert (identifier.identifier == "ite", s"Expecting `ite` but got `${identifier}`")
        IfThenElse(condition, left, right)
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

// What is implemented is:
// - Remove direct left recursion: https://en.wikipedia.org/wiki/Left_recursion#Removing_direct_left_recursion
// - Algol 60 grammar: https://www.cs.unc.edu/~plaisted/comp455/Algol60.pdf
// - Arithmetic expression: https://stackoverflow.com/a/34603099
// - The parser and the lexer: http://enear.github.io/2016/03/31/parser-combinators/

// Other helpful resources:
// Bool and arithmetic expression: https://compilers.iecc.com/crenshaw/tutor6.txt
// Bool expression: http://www.cs.unb.ca/~wdu/cs4613/a2ans.htm
// https://stackoverflow.com/a/2976708
// https://stackoverflow.com/a/43971433
// https://bitwalker.org/posts/2013-08-10-learn-by-example-scala-parser-combinators/
// http://matt.might.net/articles/implementation-of-m-expression-parser-in-scala-combinators-without-stdlexical-stdtokenparsers/