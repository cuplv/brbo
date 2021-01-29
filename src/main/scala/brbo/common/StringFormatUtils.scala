package brbo.common

object StringFormatUtils {
  def threeDigits(d: Double): String = "%.3f" format d
}
