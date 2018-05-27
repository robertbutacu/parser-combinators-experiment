package data.operations

import data.Parser

import scala.util.Try

trait ParserComposer {
  def andThen(p: Parser): Try[String]
}

object ParserComposer {
  implicit class ImplicitParserComposer(p: Parser) extends ParserComposer {
    override def andThen(p: Parser): Try[String] = ???
  }
}
