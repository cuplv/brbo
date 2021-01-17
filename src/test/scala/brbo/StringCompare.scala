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

  def ignoreWhitespaces(actual: Iterable[Any], expected: String): Boolean = ignoreWhitespaces(Left(actual), expected)

  def ignoreWhitespaces(actual: Traversable[Any], expected: String): Boolean = ignoreWhitespaces(Right(actual), expected)

  private def ignoreWhitespaces(actual: Either[Iterable[Any], Traversable[Any]], expected: String): Boolean = {
    ignoreWhitespaces(toSortedString(actual), expected)
  }

  def toSortedString(iterableOrTraversable: Either[Iterable[Any], Traversable[Any]]): String = {
    iterableOrTraversable match {
      case Left(iterable) => iterable.map(x => x.toString).toList.sortWith(_ < _).mkString("\n")
      case Right(traversable) => traversable.map(x => x.toString).toList.sortWith(_ < _).mkString("\n")
    }
  }
}
