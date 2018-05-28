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


  lazy val AParser: Parser = BasicConcepts.parseChar('A')
  lazy val BParser: Parser = BasicConcepts.parseChar('B')

  lazy val aAndBParser: Parser = AParser >> BParser
  lazy val aOrElseBParser: Parser = AParser orElse BParser


  lazy val parser: String => Try[String] = BasicConcepts.run(aAndBParser)(_)
  lazy val orElseParser: String => Try[String] = BasicConcepts.run(aOrElseBParser)(_)



  "Given a good string" should "parse 2 letter with andThen" in {
    assert(parser(goodString) match {
      case Success(_) => true
      case _ => false
    })
  }

  "Given a bad first letter string" should "not parse andThen" in {
    assert(parser(badString) match {
      case Failure(_) => true
      case _ => false
    })
  }

  "Given a bad second letter string" should "not parse andThen" in {
    assert(parser(badString2) match {
      case Failure(_) => true
      case _ => false
    })
  }

  "Given a string that can be parse initially" should "use the first parser" in {
    assert(orElseParser(goodString) match {
      case Success(_) => true
      case _ => false
    })
  }


  "Given a string that can be parse using the secondary function" should "use the second parser" in {
    assert(orElseParser(goodSecondLetterString) match {
      case Success(_) => true
      case _ => false
    })
  }

  "Given a string that cant be parser " should "throw exception " in {
    assert(orElseParser(badString) match {
      case Success(_) => false
      case Failure(_) => true
    })
  }
}
