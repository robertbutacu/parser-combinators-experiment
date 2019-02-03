package data

case class Result[A, B](value: A, remaining: B)
