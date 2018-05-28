package courses.first

import data.Parser
import org.scalatest.FlatSpec

import scala.util.{Failure, Success, Try}

class BasicConceptsTests extends FlatSpec {
  lazy val goodString = "ABC"
  lazy val badString = "ZBC"
  lazy val customLetter = 'A'
  lazy val aParser: String => Try[String] = BasicConcepts.parseChar2('A')
  lazy val dataStructureParser: Parser = BasicConcepts.buildParser('A')
  lazy val parserExecutor: String => Try[String] = BasicConcepts.run(dataStructureParser)

  def injectCharIntoPositiveMessage(c: Char): String =
    s"""Found $c"""

  def injectCharIntoNegativeMessage(expected: Char, got: Char): String =
    s"""Expected $expected, found $got"""

  "Give a string starting with A " should " parse the string" in {
    assert(BasicConcepts.AParser(goodString) === (true, "BC"))
  }

  "Give a string not starting with A" should "not parse the string " in {
    assert(BasicConcepts.AParser(badString) === (false, badString))
  }

  "Given a string starting with a custom letter" should "parse the string" in {
    assert(BasicConcepts.parseCharUncurried(goodString, customLetter) === (injectCharIntoPositiveMessage('A'), goodString.tail))
  }

  "Given a string not starting with a custom letter" should "not parse the string" in {
    assert(BasicConcepts.parseCharUncurried(badString, customLetter) === (injectCharIntoNegativeMessage('A', 'Z'), badString))
  }

  "Given a string starting with a custom letter on the try function " should "parse the string" in {
    assert(BasicConcepts.parseCharUncurriedTry(goodString, customLetter) === Success(goodString.tail))
  }

  "Given a string not starting with a custom letter on the try function " should "not parse the string" in {
    assert(BasicConcepts
      .parseCharUncurriedTry(badString, customLetter) match {
      case Failure(_) => true
      case _ => false
    })
  }

  "Give a string starting with A " should "parse it " in {
    assert(aParser(goodString) match {
      case Success(_) => true
      case _ => false
    })
  }

  "Give a string not starting with A " should "parse it using the Parser data structure" in {
    assert(aParser(badString) match {
      case Success(_) => false
      case Failure(_) => true
    })
  }


  "Give a string starting with A " should "parse it using the Parser data structure" in {
    assert(parserExecutor(goodString) match {
      case Success(_) => true
      case _ => false
    })
  }

  "Give a string not starting with A " should "parse it " in {
    assert(parserExecutor(badString) match {
      case Success(_) => false
      case Failure(_) => true
    })
  }
}
