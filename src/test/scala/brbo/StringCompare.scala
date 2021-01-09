package brbo

object StringCompare {
  def ignoreWhitespaces(s1: String, s2: String): Boolean = {
    val result = s1.replaceAll("(?s)\\s+", " ").trim == s2.replaceAll("(?s)\\s+", " ").trim
    if (!result) {
      val lineSeparator = "------------------------------------------\n"
      println(s"$lineSeparator$s1\n$lineSeparator$s2$lineSeparator")
    }
    result
  }
}
