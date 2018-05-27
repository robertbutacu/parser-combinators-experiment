package courses.first

import org.scalatest.FlatSpec

class BasicConceptsTests extends FlatSpec {
  lazy val goodString = "ABC"
  lazy val badString = "ZBC"

  "Give a string starting with A " should " parse the string" in {
    assert(BasicConcepts.AParser(goodString) === (true, "BC"))
  }

  "Give a string not starting with A" should "not parse the string " in {
    assert(BasicConcepts.AParser(badString) === (false, badString))
  }

}
