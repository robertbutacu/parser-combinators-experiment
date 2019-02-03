package courses.first

import data.{Parser, Result}
import data.operations.ParserComposer.ImplicitParserComposer

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

  def wrapWithParser(c: Char)(func: String => Try[Result[Char, String]]): Parser[Char, String] =
    Parser(c, func)

  def buildParser(c: Char): Parser[Char, String] = {
    def innerFunction(s: String): Try[Result[Char, String]] = {
      if (s.isEmpty)
        Success(Result("".toCharArray.head, "Parsed it all"))
      else {
        s.head.toLower match {
          case found if found == c.toLower => Success(Result(s.head, s.tail))
          case _ => Failure(new IllegalArgumentException(s"""Expected $c, found ${s.head}"""))
        }
      }
    }

    Parser(c, innerFunction)
  }

  def choice[A, B](parsers: List[Parser[A, B]]): Parser[A, B] =
    parsers.reduce(_ <|> _)

  def anyOf(characters: String): Parser[Char, String] = {
    choice(characters.map(c => buildParser(c)).toList)
  }

  def run(p: Parser[Char, String])(input: String) = p.func(input)
}
