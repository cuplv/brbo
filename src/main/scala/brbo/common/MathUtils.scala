package brbo.common

import org.apache.logging.log4j.LogManager

object MathUtils {
  private val logger = LogManager.getLogger("brbo.common.MathUtils")

  // Generate Cartesian product
  def crossJoin[T](list: Traversable[Traversable[T]]): List[List[T]] = {
    // Even when the input is Set, ensure the output is list-typed
    list.toList.map(e => e.toList) match {
      case Nil => Nil
      case xs :: Nil => xs map (List(_))
      case x :: xs => for {
        i <- x
        j <- crossJoin(xs)
      } yield List(i) ++ j
    }
  }

  def crossJoin2[T](list1: Traversable[T], list2: Traversable[T]): List[(T, T)] = {
    crossJoin(List(list1, list2)).map(list => (list.head, list.tail.head))
  }
}
