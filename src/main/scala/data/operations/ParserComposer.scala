package data.operations

import courses.first.BasicConcepts
import data.Parser

import scala.util.{Failure, Success, Try}

trait ParserComposer {
  def andThen(p: Parser): Parser
  def >>(p: Parser): Parser
  def orElse(p: Parser): Parser
  def choice: Try[String]
  def anyOf: Try[String]
  def run(input: String): Try[String]
  def <|>(p: Parser): Parser
}

object ParserComposer {
  implicit class ImplicitParserComposer(parser: Parser) extends ParserComposer {
    override def andThen(p: Parser): Parser = {
      def innerFnc(input: String): Try[String] = {
        val result1 = parser.run(input)

        result1 match {
          case Failure(ex) => Failure(ex)
          case Success(tail) =>
            p.run(tail)
        }
      }

      Parser(p.c, innerFnc)
    }

    override def >>(p: Parser): Parser = this.andThen(p)

    override def orElse(p: Parser): Parser = {
      def innerFnc(input: String): Try[String] = {
        val result1 = parser.run(input)

        result1 match {
          case Failure(_) => p.run(input)
          case Success(tail) => Success(tail)
        }
      }

      Parser(p.c, innerFnc)
    }

    override def choice: Try[String] = ???

    override def anyOf: Try[String] = ???

    override def run(input: String): Try[String] = parser.s(input)

    override def <|>(p: Parser): Parser = this.orElse(p)
  }
}
