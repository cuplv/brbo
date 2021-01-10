package brbo

object StringCompare {
  def ignoreWhitespaces(actual: String, expected: String): Boolean = {
    val result = actual.replaceAll("(?s)\\s+", " ").trim == expected.replaceAll("(?s)\\s+", " ").trim
    if (!result) {
      val lineSeparator = "------------------------------------------\n"
      println(s"Actual:\n$lineSeparator$actual\nExpected:\n$lineSeparator$expected\n$lineSeparator")
    }
    result
  }
}
