package courses.first

object BasicConcepts {
  def AParser(s: String): (Boolean, String) = {
    s.head.toLower match {
      case 'a' => (true, s.tail)
      case _ => (false, s)
    }
  }
}
