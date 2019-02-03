package data

import scala.util.Try

case class Parser[A, B](input: A, func: B => Try[Result[A, B]])
