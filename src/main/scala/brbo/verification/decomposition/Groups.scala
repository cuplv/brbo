package brbo.verification.decomposition

case class Groups[T <: Segment](elements: Set[T]) {
  def toTestString: String = elements.toList.map({ group => group.toTestString }).sorted.mkString("\n")
}
