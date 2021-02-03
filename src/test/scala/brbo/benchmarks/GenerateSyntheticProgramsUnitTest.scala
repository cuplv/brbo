package brbo.benchmarks

import brbo.StringCompare
import org.apache.logging.log4j.LogManager
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashSet

class GenerateSyntheticProgramsUnitTest extends AnyFlatSpec {
  private val logger = LogManager.getLogger(classOf[GenerateSyntheticProgramsUnitTest])

  "Print ASTs" should "be correct" in {
    val command = Command("R", Symbol(Left(1)))
    val command2 = Command("R", Symbol(Right("entry1")))

    val header1 = BasicHeader("i1", "n")
    val header2 = IteratorHeader("it1", "n", "entry1")

    val loopBody = Statement(List(command))
    val loop1 = Loop(header1, loopBody)
    val loop2 = Loop(header2, loopBody)
    val loop3 = Loop(header2, Statement(List(command2)))

    val expected1 =
      """for (int i1 = 0; i1 < n; i1++) {
        |    R = R + 1;
        |}""".stripMargin
    val expected2 =
      """  for (int it1 = n, entry1 = ndInt2(1, it1); it1 > 0; it1 -= entry1, entry1 = ndInt2(1, it1)) {
        |    R = R + 1;
        |  }""".stripMargin
    val expected3 =
      """    for (int it1 = n, entry1 = ndInt2(1, it1); it1 > 0; it1 -= entry1, entry1 = ndInt2(1, it1)) {
        |      R = R + entry1;
        |    }""".stripMargin
    assert(StringCompare.ignoreWhitespaces(loop1.toString(0), expected1, "Loop 1 failed"))
    assert(StringCompare.ignoreWhitespaces(loop2.toString(2), expected2, "Loop 2 failed"))
    assert(StringCompare.ignoreWhitespaces(loop3.toString(4), expected3, "Loop 3 failed"))
  }

  "Generate synthetic programs" should "be correct" in {
    logger.error("This is a FAKE unit test because does not assertion check!")
    GenerateSyntheticPrograms.generateSourceCode(50, 2, 3, "R", HashSet[String]("n"))
  }
}

object GenerateSyntheticProgramsUnitTest {
}
