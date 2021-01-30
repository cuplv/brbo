package brbo.common

import brbo.verification.AmortizationMode.AmortizationMode

import scala.collection.immutable.HashSet

object AnalysisResult {
  def interpretResult(b: Boolean): String = if (b) "Y" else "N"

  case class RawResult(file: String, time: Double, verified: Boolean, mode: AmortizationMode, lines: Int) {
    def toCSV: String = s"$file,$lines,${StringFormatUtils.oneDigit(time)},$verified,$mode"

    def toUnitResult: UnitResult = UnitResult(time, verified)
  }

  case class UnitResult(time: Double, verified: Boolean)

  case class AggregatedResult(no: UnitResult, selective: UnitResult, full: UnitResult, files: Set[String], collapse: Boolean, lines: Int) {
    def updateSingle(result: RawResult, file: String): AggregatedResult = {
      val time = result.time
      result.mode match {
        case brbo.verification.AmortizationMode.NO_AMORTIZE =>
          AggregatedResult(UnitResult(no.time + time, no.verified), selective, full, files + file, collapse, result.lines + this.lines)
        case brbo.verification.AmortizationMode.FULL_AMORTIZE =>
          AggregatedResult(no, selective, UnitResult(full.time + time, full.verified), files + file, collapse, result.lines + this.lines)
        case brbo.verification.AmortizationMode.SELECTIVE_AMORTIZE =>
          AggregatedResult(no, UnitResult(selective.time + time, selective.verified), full, files + file, collapse, result.lines + this.lines)
        case _ => throw new Exception("Unexpected")
      }
    }

    def updateSequence(analysisResults: Iterable[RawResult], file: String): AggregatedResult = {
      var r = this
      analysisResults.foreach({
        result => r = r.updateSingle(result, file)
      })
      r
    }

    def toCSV: String = {
      val name =
        if (collapse) s"${files.size}"
        else {
          assert(files.size == 1)
          s"${files.head}"
        }
      s"$name,$lines,${interpretResult(no.verified)},${StringFormatUtils.oneDigit(no.time)}," +
        s"${interpretResult(full.verified)},${StringFormatUtils.oneDigit(full.time)}," +
        s"${interpretResult(selective.verified)},${StringFormatUtils.oneDigit(selective.time)}"
    }
  }

  def aggregateResultsIndividual(results: List[List[RawResult]]): List[AggregatedResult] = {
    results.map({
      result =>
        assert(result.size == 3)
        val no = result.head
        val selective = result(1)
        val full = result(2)
        assert(no.file == selective.file && selective.file == full.file)
        assert(no.lines == selective.lines && selective.lines == full.lines)
        AggregatedResult(no.toUnitResult, selective.toUnitResult, full.toUnitResult, HashSet[String](no.file), collapse = false, no.lines)
    })
  }

  def aggregateResultsSummary(results: List[List[RawResult]]): List[AggregatedResult] = {
    var r1 = AggregatedResult(UnitResult(0, verified = true), UnitResult(0, verified = true), UnitResult(0, verified = true), new HashSet[String], collapse = true, 0)
    var r2 = AggregatedResult(UnitResult(0, verified = true), UnitResult(0, verified = true), UnitResult(0, verified = false), new HashSet[String], collapse = true, 0)
    var r3 = AggregatedResult(UnitResult(0, verified = true), UnitResult(0, verified = false), UnitResult(0, verified = true), new HashSet[String], collapse = true, 0)
    var r4 = AggregatedResult(UnitResult(0, verified = true), UnitResult(0, verified = false), UnitResult(0, verified = false), new HashSet[String], collapse = true, 0)
    var r5 = AggregatedResult(UnitResult(0, verified = false), UnitResult(0, verified = true), UnitResult(0, verified = true), new HashSet[String], collapse = true, 0)
    var r6 = AggregatedResult(UnitResult(0, verified = false), UnitResult(0, verified = true), UnitResult(0, verified = false), new HashSet[String], collapse = true, 0)
    var r7 = AggregatedResult(UnitResult(0, verified = false), UnitResult(0, verified = false), UnitResult(0, verified = true), new HashSet[String], collapse = true, 0)
    var r8 = AggregatedResult(UnitResult(0, verified = false), UnitResult(0, verified = false), UnitResult(0, verified = false), new HashSet[String], collapse = true, 0)
    results.foreach({
      result =>
        assert(result.size == 3)
        val no = result.head
        val selective = result(1)
        val full = result(2)
        assert(no.file == selective.file && selective.file == full.file)
        assert(no.lines == selective.lines && selective.lines == full.lines)
        (no.verified, selective.verified, full.verified) match {
          case (true, true, true) => r1 = r1.updateSequence(result, no.file)
          case (true, true, false) => r2 = r2.updateSequence(result, no.file)
          case (true, false, true) => r3 = r3.updateSequence(result, no.file)
          case (true, false, false) => r4 = r4.updateSequence(result, no.file)
          case (false, true, true) => r5 = r5.updateSequence(result, no.file)
          case (false, true, false) => r6 = r6.updateSequence(result, no.file)
          case (false, false, true) => r7 = r7.updateSequence(result, no.file)
          case (false, false, false) => r8 = r8.updateSequence(result, no.file)
        }
    })
    List[AggregatedResult](r1, r2, r3, r4, r5, r6, r7, r8)
  }
}
