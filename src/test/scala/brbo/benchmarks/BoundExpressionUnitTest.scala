package brbo.benchmarks

import brbo.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class BoundExpressionUnitTest extends AnyFlatSpec {
  "Bound generation" should "be correct" in {
    val test01 = {
      val loop1 = Loop(
        IteratorHeader("it1", "entry0", "entry1"),
        Statement(
          List[BasicStatement](
            Command("R", Symbol(Left(1))),
            Command("R", Symbol(Right("n")))
          )
        )
      )
      val loop2 = Loop(
        IteratorHeader("it0", "n", "entry0"),
        Statement(List[BasicStatement](loop1))
      )
      loop2
    }
    val result1 = GenerateSyntheticPrograms.generateBound(test01)
    assert(StringCompare.ignoreWhitespaces(result1.toString, "(1 * n + n * n)", "Test 01 failed"))
  }
}

object BoundExpressionUnitTest {
}
