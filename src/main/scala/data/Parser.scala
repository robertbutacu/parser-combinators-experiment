package data

import scala.util.Try

case class Parser(c: Char, s: String => Try[String])
