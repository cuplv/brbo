package brbo

object StringCompare {
  def ignoreWhitespaces(actual: String, expected: String, message: String): Boolean = {
    val result = actual.replaceAll("(?s)\\s+", " ").trim == expected.replaceAll("(?s)\\s+", " ").trim
    if (!result) {
      val lineSeparator = "------------------------------------------\n"
      println(s"Error message: $message\nActual:\n$lineSeparator$actual\nExpected:\n$lineSeparator$expected\n$lineSeparator")
    }
    result
  }

  def ignoreWhitespaces(actual: Iterable[Any], expected: String, message: String): Boolean = ignoreWhitespaces(Left(actual), expected, message)

  def ignoreWhitespaces(actual: Traversable[Any], expected: String, message: String): Boolean = ignoreWhitespaces(Right(actual), expected, message)

  private def ignoreWhitespaces(actual: Either[Iterable[Any], Traversable[Any]], expected: String, message: String): Boolean = {
    ignoreWhitespaces(toSortedString(actual), expected, message)
  }

  def toSortedString(iterableOrTraversable: Either[Iterable[Any], Traversable[Any]]): String = {
    iterableOrTraversable match {
      case Left(iterable) => iterable.map(x => x.toString).toList.sortWith(_ < _).mkString("\n")
      case Right(traversable) => traversable.map(x => x.toString).toList.sortWith(_ < _).mkString("\n")
    }
  }
}
