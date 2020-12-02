package brbo.common.icra

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object IcraLexer extends RegexParsers {

  sealed trait IcraToken

  case class IDENTIFIER(identifier: String) extends IcraToken

  case class NUMBER(number: Int) extends IcraToken

  case class AND() extends IcraToken

  case class OR() extends IcraToken

  case class NEGATION() extends IcraToken

  case class PLUS() extends IcraToken

  case class MINUS() extends IcraToken

  case class MULTIPLICATION() extends IcraToken

  case class LEFT_BRACKET() extends IcraToken

  case class RIGHT_BRACKET() extends IcraToken

  case class EQUAL() extends IcraToken

  case class LESS_THAN() extends IcraToken

  case class LESS_THAN_OR_EQUAL_TO() extends IcraToken

  case class GREATER_THAN() extends IcraToken

  case class GREATER_THAN_OR_EQUAL_TO() extends IcraToken

  case class ASSIGNMENT() extends IcraToken

  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f\n]+".r

  private def identifier: Parser[IDENTIFIER] = {
    val javaIdentifier = "[a-zA-Z_][a-zA-Z0-9_]*"
    s"$javaIdentifier'?:?[0-9]*".r ^^ { string => IDENTIFIER(string) }
  }

  private def number: Parser[NUMBER] = {
    s"[0-9]+".r ^^ { string => NUMBER(string.toInt) }
  }

  private def and = """/\""" ^^ (_ => AND)

  private def or = """\/""" ^^ (_ => OR)

  private def negation = """!""" ^^ (_ => NEGATION)

  private def plus = """+""" ^^ (_ => PLUS)

  private def minus = """-""" ^^ (_ => MINUS)

  private def multiplication = """*""" ^^ (_ => MULTIPLICATION)

  private def leftBracket = """(""" ^^ (_ => LEFT_BRACKET)

  private def rightBracket = """)""" ^^ (_ => RIGHT_BRACKET)

  private def equal = """=""" ^^ (_ => EQUAL)

  private def lessThan = """<""" ^^ (_ => LESS_THAN)

  private def lessThanOrEqualTo = """<=""" ^^ (_ => LESS_THAN_OR_EQUAL_TO)

  private def greaterThan = """>""" ^^ (_ => GREATER_THAN)

  private def greaterThanOrEqualTo = """>=""" ^^ (_ => GREATER_THAN_OR_EQUAL_TO)

  private def assignment = """:=""" ^^ (_ => ASSIGNMENT)

  private def tokens: Parser[List[Object]] = {
    phrase(rep1(and | or | negation | plus | minus | multiplication | leftBracket | rightBracket
      | lessThanOrEqualTo | greaterThanOrEqualTo | lessThan | greaterThan
      | assignment | equal
      | number | identifier))
  }

  def parse(code: String): List[Object] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => throw new RuntimeException(s"Error when applying a lexer on invariant string `$code`: $msg")
      case Success(result, next) => result
    }
  }
}