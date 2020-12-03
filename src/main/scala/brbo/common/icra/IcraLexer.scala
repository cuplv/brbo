package brbo.common.icra

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object IcraLexer extends RegexParsers {

  sealed trait IcraToken

  case class IDENTIFIER(identifier: String) extends IcraToken {
    override def toString: String = identifier
  }

  case class NUMBER(number: Int) extends IcraToken {
    override def toString: String = number.toString
  }

  object AND extends IcraToken {
    override def toString: String = """/\"""
  }

  object OR extends IcraToken {
    override def toString: String = """\/"""
  }

  object NEGATION extends IcraToken {
    override def toString: String = "!"
  }

  object PLUS extends IcraToken {
    override def toString: String = "+"
  }

  object MINUS extends IcraToken {
    override def toString: String = "-"
  }

  object MULTIPLICATION extends IcraToken {
    override def toString: String = "*"
  }

  object DIVISION extends IcraToken {
    override def toString: String = """/"""
  }

  object LEFT_BRACKET extends IcraToken {
    override def toString: String = "("
  }

  object RIGHT_BRACKET extends IcraToken {
    override def toString: String = ")"
  }

  object EQUAL extends IcraToken {
    override def toString: String = "="
  }

  object LESS_THAN extends IcraToken {
    override def toString: String = "<"
  }

  object LESS_THAN_OR_EQUAL_TO extends IcraToken {
    override def toString: String = "<="
  }

  object GREATER_THAN extends IcraToken {
    override def toString: String = ">"
  }

  object GREATER_THAN_OR_EQUAL_TO extends IcraToken {
    override def toString: String = ">="
  }

  object ASSIGNMENT extends IcraToken {
    override def toString: String = ":="
  }

  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f\n]+".r

  private def identifier: Parser[IDENTIFIER] = {
    val javaIdentifier = "[a-zA-Z_][a-zA-Z0-9_]*"
    s"$javaIdentifier'?:?[0-9]*".r ^^ { string => IDENTIFIER(string) }
  }

  private def number: Parser[NUMBER] = {
    s"(\\d+)".r ^^ { string => NUMBER(string.toInt) }
  }

  private def and: Parser[IcraToken] = """/\""" ^^ (_ => AND)

  private def or: Parser[IcraToken] = """\/""" ^^ (_ => OR)

  private def negation: Parser[IcraToken] = """!""" ^^ (_ => NEGATION)

  private def plus: Parser[IcraToken] = """+""" ^^ (_ => PLUS)

  private def minus: Parser[IcraToken] = """-""" ^^ (_ => MINUS)

  private def multiplication : Parser[IcraToken]= """*""" ^^ (_ => MULTIPLICATION)

  private def division : Parser[IcraToken]= """/""" ^^ (_ => DIVISION)

  private def leftBracket: Parser[IcraToken] = """(""" ^^ (_ => LEFT_BRACKET)

  private def rightBracket: Parser[IcraToken] = """)""" ^^ (_ => RIGHT_BRACKET)

  private def equal: Parser[IcraToken] = """=""" ^^ (_ => EQUAL)

  private def lessThan: Parser[IcraToken] = """<""" ^^ (_ => LESS_THAN)

  private def lessThanOrEqualTo: Parser[IcraToken] = """<=""" ^^ (_ => LESS_THAN_OR_EQUAL_TO)

  private def greaterThan: Parser[IcraToken] = """>""" ^^ (_ => GREATER_THAN)

  private def greaterThanOrEqualTo : Parser[IcraToken]= """>=""" ^^ (_ => GREATER_THAN_OR_EQUAL_TO)

  private def assignment : Parser[IcraToken]= """:=""" ^^ (_ => ASSIGNMENT)

  private def tokens: Parser[List[IcraToken]] = {
    phrase(
      rep1(
        and | or | negation | plus | minus | multiplication | division | leftBracket | rightBracket
      | lessThanOrEqualTo | greaterThanOrEqualTo | lessThan | greaterThan
      | assignment | equal
      | number | identifier
      )
    )
  }

  def parse(code: String): List[IcraToken] = {
    parse(tokens, code) match {
      case NoSuccess(message, next) => throw new RuntimeException(s"Error when applying a lexer on string `$code`: $message")
      case Success(result, next) => result
    }
  }
}