package data.operations

import courses.first.BasicConcepts
import data.Parser
import org.scalatest.FlatSpec
import data.operations.ParserComposer.ImplicitParserComposer

import scala.util.{Failure, Success, Try}

class ParserComposerTest extends FlatSpec {
  lazy val goodString = "ABC"
  lazy val goodSecondLetterString = "BBC"
  lazy val badString = "ZBC"
  lazy val badString2 = "AZC"
  lazy val customLetter = 'A'

  lazy val AParser: Parser[Char, String] = BasicConcepts.buildParser('A')
  lazy val BParser: Parser[Char, String] = BasicConcepts.buildParser('B')

  lazy val aAndBParser: Parser[Char, String]    = AParser >> BParser
  lazy val aOrElseBParser: Parser[Char, String] = AParser orElse BParser

  "Given a list of random chars" should "parse any of them " in {
    assert(AParser.anyOf(List('A', 'B', 'C', 'D', 'F')).run("AAABBBCDFFF") match {
      case Success(r) => println(r); true
      case _          => false
    })
  }

  "Given a good string" should "parse 2 letter with andThen" in {
    assert(aAndBParser.run(goodString) match {
      case Success(_) => true
      case _ => false
    })
  }

  "Given a bad first letter string" should "not parse andThen" in {
    assert(aAndBParser.run(badString) match {
      case Failure(_) => true
      case _ => false
    })
  }

  "Given a bad second letter string" should "not parse andThen" in {
    assert(aAndBParser.run(badString2) match {
      case Failure(_) => true
      case _ => false
    })
  }

  "Given a string that can be parse initially" should "use the first parser" in {
    assert(aOrElseBParser.run(goodString) match {
      case Success(_) => true
      case _ => false
    })
  }


  "Given a string that can be parse using the secondary function" should "use the second parser" in {
    assert(aOrElseBParser.run(goodSecondLetterString) match {
      case Success(_) => true
      case _ => false
    })
  }

  "Given a string that cant be parser " should "throw exception " in {
    assert(aOrElseBParser.run(badString) match {
      case Success(_) => false
      case Failure(_) => true
    })
  }
}
