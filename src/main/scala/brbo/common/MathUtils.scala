package brbo.common

object MathUtils {
  // Generate Cartesian product
  def crossJoin[T](list: Traversable[Traversable[T]]): Traversable[Traversable[T]] = {
    // Even when the input is Set, ensure the output is list-typed
    list.toList.map(e => e.toList) match {
      case Nil => Nil
      case xs :: Nil => xs map (Traversable(_))
      case x :: xs => for {
        i <- x
        j <- crossJoin(xs)
      } yield Traversable(i) ++ j
    }
  }

  def crossJoin2[T](list1: Traversable[T], list2: Traversable[T]): Traversable[(T, T)] = {
    ???
  }
}
