package data.operations

import courses.first.BasicConcepts
import data.{Parser, Result}

import scala.util.{Failure, Success, Try}

trait ParserComposer[A, B] {
  def andThen(p: Parser[A, B]): Parser[A, B]
  def >>(p: Parser[A, B]): Parser[A, B] // andThen sugar
  def orElse(p: Parser[A, B]): Parser[A, B]
  def choice(otherParsers: List[Parser[A, B]]): Parser[A, B]
  def anyOf(ls: List[A]): Parser[A, B]
  def run(input: B): Try[Result[A, B]]
  def <|>(p: Parser[A, B]): Parser[A, B] // orElse sugar
  def map[C](f: A => C): Parser[C, B]
  def <%>[C](f: A => C): Parser[C, B] // map sugar
}

object ParserComposer {
  implicit class ImplicitParserComposer[A, B](parser: Parser[A, B]) extends ParserComposer[A, B] {
    override def andThen(p: Parser[A, B]): Parser[A, B] = {
      def innerFnc(input: B): Try[Result[A, B]] = {
        val result1 = parser.run(input)

        result1.flatMap(r => p.run(r.remaining))
      }

      Parser(p.input, innerFnc)
    }

    override def >>(p: Parser[A, B]): Parser[A, B]     = this.andThen(p)

    override def orElse(p: Parser[A, B]): Parser[A, B] = {
      def innerFnc(input: B): Try[Result[A, B]] = {
        val result1 = parser.run(input)

        result1 match {
          case Failure(_) => p.run(input)
          case Success(tail) => Success(tail)
        }
      }

      Parser(p.input, innerFnc)
    }

    override def choice(otherParsers: List[Parser[A, B]]): Parser[A, B]      = (otherParsers :+ parser).reduce(_ <|> _)

    override def anyOf(ls: List[A]): Parser[A, B] = ls.map(a => Parser(a, parser.func)).reduce(_ <|> _)

    override def run(input: B): Try[Result[A, B]]   = parser.func(input)

    override def <|>(p: Parser[A, B]): Parser[A, B] = this.orElse(p)

    override def map[C](f: A => C): Parser[C, B]    = {
      def mapOverParser(input: B): Try[Result[C, B]] = {
        parser.run(input).map(r => r.copy(value = f(r.value)))
      }

      Parser(f(parser.input), mapOverParser)
    }

    override def <%>[C](f: A => C): Parser[C, B] = this.map(f)
  }
}
