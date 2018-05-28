package data.operations

import courses.first.BasicConcepts
import data.Parser

import scala.util.{Failure, Success, Try}

trait ParserComposer {
  def andThen(p: Parser): Parser
  def >>(p: Parser): Parser
  def orElse(p: Parser): Try[String]
  def choice: Try[String]
  def anyOf: Try[String]
}

object ParserComposer {
  implicit class ImplicitParserComposer(parser: Parser) extends ParserComposer {
    override def andThen(p: Parser): Parser = {
      def innerFnc(input: String): Try[String] = {
        val result1 = BasicConcepts.run(parser)(input)

        result1 match {
          case Failure(ex) => Failure(ex)
          case Success(tail) =>
            BasicConcepts.run(p)(tail)
        }
      }

      Parser(p.c, innerFnc)
    }

    override def >>(p: Parser): Parser = this.andThen(p)

    override def orElse(p: Parser): Try[String] = ???

    override def choice: Try[String] = ???

    override def anyOf: Try[String] = ???
  }
}
