package courses.first

import data.Parser

import scala.util.{Failure, Success, Try}

object BasicConcepts {
  type Message = String

  def AParser(s: String): (Boolean, String) = {
    s.head.toLower match {
      case 'a' => (true, s.tail)
      case _ => (false, s)
    }
  }

  def parseCharUncurried(s: String, c: Char): (Message, String) = {
    if (s.isEmpty)
      ("Parsed the whole string", "")
    else {
      s.head.toLower match {
        case found if found == c.toLower => (s"""Found $c""", s.tail)
        case _ => (s"""Expected $c, found ${s.head}""", s)
      }
    }
  }

  def parseCharUncurriedTry(s: String, c: Char): Try[String] = {
    if (s.isEmpty)
      Success("Parsed the whole string")
    else {
      s.head.toLower match {
        case found if found == c.toLower => Success(s.tail)
        case _ => Failure(new IllegalArgumentException(s"""Expected $c, found ${s.head}"""))
      }
    }
  }

  def parseChar2(c: Char)(s: String): Try[String] = {
    if (s.isEmpty)
      Success("Parsed the whole string")
    else {
      s.head.toLower match {
        case found if found == c.toLower => Success(s.tail)
        case _ => Failure(new IllegalArgumentException(s"""Expected $c, found ${s.head}"""))
      }
    }
  }

  def wrapWithParser(c: Char)(func: String => Try[String]): Parser =
    Parser(c, func)

  def parseChar(c: Char): Parser = {
    def innerFunction(s: String): Try[String] = {
      if (s.isEmpty)
        Success("Parsed the whole string")
      else {
        s.head.toLower match {
          case found if found == c.toLower => Success(s.tail)
          case _ => Failure(new IllegalArgumentException(s"""Expected $c, found ${s.head}"""))
        }
      }
    }

    Parser(c, innerFunction)
  }

  def run(p: Parser)(input: String) = p.s(input)
}
