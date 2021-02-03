package brbo.benchmarks

import scala.collection.immutable.HashSet

sealed trait Expr

case class Number(value: Int) extends Expr {
  override def toString: String = value.toString
}

case class Identifier(name: String) extends Expr {
  override def toString: String = name
}

case class AddExpr(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"(${left.toString} + ${right.toString})"
}

case class MulExpr(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"${left.toString} * ${right.toString}"
}

object BoundExpression {
  def substituteBaseExpression(expression: Expr, from: Expr, to: Expr): Expr = {
    expression match {
      case _@(Number(_) | Identifier(_)) =>
        if (expression == from) to
        else expression
      case AddExpr(left, right) => AddExpr(substituteBaseExpression(left, from, to), substituteBaseExpression(right, from, to))
      case MulExpr(left, right) => MulExpr(substituteBaseExpression(left, from, to), substituteBaseExpression(right, from, to))
    }
  }

  def flattenToSumOfMulExpr(expression: Expr, multipliedBy: Option[Expr]): Expr = {
    expression match {
      case _@(Number(_) | Identifier(_)) =>
        multipliedBy match {
          case Some(value) => MulExpr(expression, value)
          case None => expression
        }
      case AddExpr(left, right) => AddExpr(flattenToSumOfMulExpr(left, multipliedBy), flattenToSumOfMulExpr(right, multipliedBy))
      case MulExpr(left, right) => flattenToSumOfMulExpr(left, Some(right))
    }
  }

  def collectBaseExpr(expression: Expr): Set[Expr] = {
    expression match {
      case _@(Number(_) | Identifier(_)) => HashSet[Expr](expression)
      case AddExpr(left, right) => collectBaseExpr(left) ++ collectBaseExpr(right)
      case MulExpr(left, right) => collectBaseExpr(left) ++ collectBaseExpr(right)
    }
  }

  def getRidOfZeroItems(expression: Expr): Expr = {
    expression match {
      case _@(Number(_) | Identifier(_)) => expression
      case AddExpr(left, right) =>
        val newLeft = getRidOfZeroItems(left)
        val newRight = getRidOfZeroItems(right)
        (newLeft == Number(0), newRight == Number(0)) match {
          case (true, true) => Number(0)
          case (true, false) => newRight
          case (false, true) => newLeft
          case (false, false) => AddExpr(newLeft, newRight)
        }
        // Guarantee: There is no _ + 0 or 0 + _ in subexpressions
      case MulExpr(left, right) =>
        val newLeft = getRidOfZeroItems(left)
        val newRight = getRidOfZeroItems(right)
        (newLeft == Number(0), newRight == Number(0)) match {
          case (false, false) => MulExpr(newLeft, newRight)
          case _ => Number(0)
        }
        // Guarantee: There is no _ * 0 or 0 * _ in subexpressions
    }
  }
}