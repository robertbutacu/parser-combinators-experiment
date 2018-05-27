package data

import scala.util.Try

case class Parser(s: String => Try[String])
