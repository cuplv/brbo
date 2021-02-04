package brbo.common

object StringFormatUtils {
  def float(d: Double, digit: Int = 1): String = s"%.${digit}f" format d

  def integer(i: Int, digit: Int): String = s"%0${digit}d".format(i)
}
