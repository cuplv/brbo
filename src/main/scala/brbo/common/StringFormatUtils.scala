package brbo.common

object StringFormatUtils {
  def oneDigit(d: Double): String = "%.1f" format d
}
