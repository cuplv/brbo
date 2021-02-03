package brbo.common

object StringFormatUtils {
  def oneDigit(d: Double): String = "%.1f" format d

  def threeDigit(i: Int): String = "%03d".format(i)
}
