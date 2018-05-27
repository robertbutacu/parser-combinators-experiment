package courses.first

import org.scalatest.FlatSpec

import scala.util.{Failure, Success}

class BasicConceptsTests extends FlatSpec {
  lazy val goodString = "ABC"
  lazy val badString = "ZBC"
  lazy val customLetter = 'A'

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
}
