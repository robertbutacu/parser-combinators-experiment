package data

import scala.util.Try

case class Parser[A, B](c: A, s: B => Try[Result[A, B]])
