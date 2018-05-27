package data.operations

import data.Parser

import scala.util.Try

trait ParserComposer {
  def andThen(p: Parser): Try[String]
  def >>(p: Parser): Try[String]
}

object ParserComposer {
  implicit class ImplicitParserComposer(parser: Parser) extends ParserComposer {
    override def andThen(p: Parser): Try[String] = ???

    override def >>(p: Parser): Try[String] = this.>>(p)
  }
}
