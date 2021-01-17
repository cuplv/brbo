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

  def ignoreWhitespaces(actual: Iterable[Any], expected: String): Boolean = {
    ignoreWhitespaces(toSortedString(actual), expected)
  }

  def toSortedString(iterable: Iterable[Any]): String = {
    iterable.map(x => x.toString).toList.sortWith(_ < _).toString()
  }
}
