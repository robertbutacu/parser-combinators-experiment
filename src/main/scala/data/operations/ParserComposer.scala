package data.operations

import data.{Parser, Result}

import scala.util.{Failure, Success, Try}

trait ParserComposer[A, B] {
  def andThen(p: Parser[A, B]): Parser[A, B]
  def >>(p: Parser[A, B]): Parser[A, B] // andThen sugar
  def orElse(p: Parser[A, B]): Parser[A, B]
  def choice: Try[Result[A, B]]
  def anyOf: Try[Result[A, B]]
  def run(input: B): Try[Result[A, B]]
  def <|>(p: Parser[A, B]): Parser[A, B] // orElse sugar
  def map[C](f: A => C): Parser[C, B]
  def <!>[C](f: A => C): Parser[C, B] // map sugar
}

object ParserComposer {
  implicit class ImplicitParserComposer[A, B](parser: Parser[A, B]) extends ParserComposer[A, B] {
    override def andThen(p: Parser[A, B]): Parser[A, B] = {
      def innerFnc(input: B): Try[Result[A, B]] = {
        val result1 = parser.run(input)

        result1.flatMap(r => p.run(r.remaining))
      }

      Parser(p.c, innerFnc)
    }

    override def >>(p: Parser[A, B]): Parser[A, B] = this.andThen(p)

    override def orElse(p: Parser[A, B]): Parser[A, B] = {
      def innerFnc(input: B): Try[Result[A, B]] = {
        val result1 = parser.run(input)

        result1 match {
          case Failure(_) => p.run(input)
          case Success(tail) => Success(tail)
        }
      }

      Parser(p.c, innerFnc)
    }

    override def choice: Try[Result[A, B]] = ???

    override def anyOf: Try[Result[A, B]] = ???

    override def run(input: B): Try[Result[A, B]] = parser.s(input)

    override def <|>(p: Parser[A, B]): Parser[A, B] = this.orElse(p)

    override def map[C](f: A => C): Parser[C, B] = {
      def mapOverParser(input: B): Try[Result[C, B]] = {
        parser.run(input).map(r => r.copy(value = f(r.value)))
      }

      Parser(f(parser.c), mapOverParser)
    }

    override def <!>[C](f: A => C): Parser[C, B] = this.map(f)
  }
}
