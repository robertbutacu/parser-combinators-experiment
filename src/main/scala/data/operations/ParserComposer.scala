package data.operations

import data.Parser

import scala.util.Try

trait ParserComposer {
  def andThen(p: Parser): Try[String]
  def >>(p: Parser): Try[String]
  def orElse(p: Parser): Try[String]
  def choice: Try[String]
  def anyOf: Try[String]
}

object ParserComposer {
  implicit class ImplicitParserComposer(parser: Parser) extends ParserComposer {
    override def andThen(p: Parser): Try[String] = ???

    override def >>(p: Parser): Try[String] = this.>>(p)

    override def orElse(p: Parser): Try[String] = ???

    override def choice: Try[String] = ???

    override def anyOf: Try[String] = ???
  }
}
