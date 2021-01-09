package brbo.common

object StringCompare {
  def ignoreWhitespaces(s1: String, s2: String): Boolean = {
    s1.replaceAll("(?s)\\s+", " ").trim == s2.replaceAll("(?s)\\s+", " ").trim
  }
}
