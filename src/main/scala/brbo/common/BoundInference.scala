package brbo.common

import brbo.common.BoundInference.Polynomial
import brbo.common.TypeUtils.BrboType.INT
import brbo.common.icra.Icra
import com.microsoft.z3.{BoolExpr, Expr}
import org.apache.logging.log4j.LogManager

import scala.collection.immutable.HashSet

class BoundInference(targetMethod: TargetMethod, arguments: CommandLineArguments) {
  private val logger = BoundInference.logger
  private val MAX_COEFFICIENT = 8

  def inferBound(solver: Z3Solver, locations: Locations, whichVariable: String, maxDegree: Int): BoolExpr = {
    logger.info(s"Max degree: `$maxDegree`; Max coefficient: `$MAX_COEFFICIENT`")
    val maxPolynomial = {
      val integerTyped = targetMethod.inputVariables.filter(pair => pair._2 == INT)
      BoundInference.generateTemplateInvariant(maxDegree, MAX_COEFFICIENT, integerTyped.keySet.toList)
    }
    var result = maxPolynomial

    val existsUpperBound = checkAGuess(locations, maxPolynomial, whichVariable)

    var degree = maxDegree
    while (degree >= 0) {
      logger.trace(s"Binary search coefficients for degree $degree")

      maxPolynomial.monomials(degree).zipWithIndex.foreach({
        case (_, index) =>
          if (existsUpperBound) {
            val newPolynomial = result.updateCoefficient(degree, index, 0)
            if (checkAGuess(locations, newPolynomial, whichVariable)) {
              logger.trace(s"We can eliminate polynomial ${result.monomials(degree)(index)}")
              result = newPolynomial
            }
            else {
              logger.trace(s"We cannot eliminate polynomial ${result.monomials(degree)(index)}")
              binarySearchUpperBound(locations, degree, index, 1, MAX_COEFFICIENT, MAX_COEFFICIENT, maxPolynomial, whichVariable) match {
                case Some(value) => result = result.updateCoefficient(degree, index, value)
                case None => result = result.updateCoefficient(degree, index, MAX_COEFFICIENT)
              }
            }
            logger.trace(s"Infer coefficient `($degree, $index)` for `${result.monomials(degree)(index)}` as `${result.coefficients(degree)(index)}`")
          }
      })

      degree = degree - 1
    }
    val boundExpression =
      if (existsUpperBound) solver.mkLe(solver.mkIntVar(whichVariable), result.toExpr(solver))
      else solver.mkTrue()
    val boundExpressionString = this.synchronized {
      boundExpression.toString
    }
    logger.error(s"Infer bound `$boundExpressionString` for variable `$whichVariable`")
    boundExpression
  }

  def checkAGuess(locations: Locations, polynomial: Polynomial, whichVariable: String): Boolean = {
    val cProgram = InvariantInference.translateToCAndInsertAssertions(targetMethod, locations, s"$whichVariable <= ${polynomial.toString}")
    if (arguments.printIcraInputs) {
      logger.error(s"ICRA input:\n$cProgram")
    }
    Icra.runAndParseAssertionChecks(cProgram, arguments.icraTimeout) match {
      case Some(checks) => checks.forall(b => b)
      case None => false
    }
  }

  def binarySearchUpperBound(locations: Locations, degree: Int, index: Int, start: Int, end: Int, maxCoefficient: Int, polynomial: Polynomial, whichVariable: String): Option[Int] = {
    if (start < 1 || end > maxCoefficient || start > end) {
      logger.trace("1")
      return None
    }

    if (start == end) {
      val newPolynomial = polynomial.updateCoefficient(degree, index, start)
      logger.trace("2")
      val result =
        if (checkAGuess(locations, newPolynomial, whichVariable)) Some(start)
        else None
      return result
    }

    {
      val newPolynomial = polynomial.updateCoefficient(degree, index, start)
      logger.trace("3")
      if (checkAGuess(locations, newPolynomial, whichVariable)) {
        return Some(start)
      }
    }

    {
      val newPolynomial = polynomial.updateCoefficient(degree, index, end)
      logger.trace("4")
      if (checkAGuess(locations, newPolynomial, whichVariable)) {
        return None
      }
    }

    {
      val mid = (start + end) / 2
      val newPolynomial = polynomial.updateCoefficient(degree, index, mid)
      if (checkAGuess(locations, newPolynomial, whichVariable)) {
        if (start + 1 > mid || start + 1 > maxCoefficient) {
          return Some(mid)
        }
        else {
          val result =
            binarySearchUpperBound(locations, degree, index, start + 1, mid, maxCoefficient, polynomial, whichVariable) match {
              case Some(value) => Some(value)
              case None => Some(mid)
            }
          return result
        }
      }
      else {
        if (mid + 1 > end || mid + 1 > maxCoefficient) {
          return Some(end)
        }
        else {
          val result =
            binarySearchUpperBound(locations, degree, index, mid + 1, end, maxCoefficient, polynomial, whichVariable) match {
              case Some(value) => Some(value)
              case None => Some(end)
            }
          return result
        }
      }
    }
  }

}

object BoundInference {
  private val logger = LogManager.getLogger("brbo.common.BoundInference")

  def generateTemplateInvariant(maxDegree: Int, maxCoefficient: Int, inputVariables: List[String]): Polynomial = {
    var monomials = List[List[Monomial]]()
    var endIndex = inputVariables.size
    while (endIndex <= inputVariables.size + maxDegree) {
      val degrees = generateDegrees(inputVariables.size, inputVariables.size + maxDegree, endIndex, List[Int]())
      val polynomials = generatePolynomials(degrees, endIndex, inputVariables)
      monomials = monomials :+ polynomials
      endIndex = endIndex + 1
    }
    // val lowerCoefficients = monomials.map(list => list.map(_ => 0))
    val upperCoefficients = monomials.map(list => list.map(_ => maxCoefficient))
    Polynomial(upperCoefficients, monomials)
  }

  def generatePolynomials(allPaths: Set[List[Int]], endIndex: Int, variables: List[String]): List[Monomial] = {
    var result = List[Monomial]()
    allPaths.foreach({
      path =>
        assert(variables.size == path.length + 1)
        var monomials = List[Monomial]()
        path.zipWithIndex.foreach({
          case (position, index) =>
            val degree = {
              if (index == 0) position - 1
              else position - path(index - 1) - 1
            }
            monomials = monomials :+ Monomial(List((variables(index), degree)))
        })
        val degree = {
          if (path.nonEmpty) endIndex - path.last - 1
          else endIndex - 1
        }
        monomials = monomials :+ Monomial(List((variables.last, degree)))

        assert(monomials.nonEmpty)
        val r = {
          if (monomials.size == 1)
            monomials.last
          else {
            var r = monomials.head
            monomials.zipWithIndex.foreach({
              case (monomial: Monomial, index) =>
                if (index > 0) {
                  r = Monomial(r.basesAndPowers ::: monomial.basesAndPowers)
                }
            })
            r
          }
        }
        result = result :+ r
    })
    result
  }

  def generateDegrees(n: Int, m: Int, endIndex: Int, path: List[Int]): Set[List[Int]] = {
    assert(n <= m && n >= 0 && m >= 0)
    var result = new HashSet[List[Int]]
    if (path.size == n - 1) {
      return result + path
    }
    val startIndex = // TODO: ???
      if (path.isEmpty) 1
      else {
        path.last + 1
      }
    var i = startIndex
    while (i < endIndex) {
      val newPath = path :+ i
      val degrees = generateDegrees(n, m, endIndex, newPath)
      result = result ++ degrees
      i = i + 1
    }
    result
  }

  case class Monomial(basesAndPowers: List[(String, Int)]) {
    basesAndPowers.foreach(pair => assert(pair._2 >= 0))

    override def toString: String = {
      basesAndPowers.map({
        case (base, power) =>
          var result = "1"
          var i = 0
          while (i < power) {
            result = s"$result * $base"
            i = i + 1
          }
          result
      }).mkString(" * ")
    }

    // To avoid crashing z3
    def toExpr(solver: Z3Solver): Expr = this.synchronized {
      var result: Expr = solver.mkIntVal(1)
      basesAndPowers.foreach({
        case (base, power) =>
          var i = 0
          while (i < power) {
            result = solver.mkMul(result, solver.mkIntVar(base))
            i = i + 1
          }
          result
      })
      result
    }
  }

  case class Polynomial(coefficients: List[List[Int]], monomials: List[List[Monomial]]) {
    override def toString: String = {
      coefficients.zip(monomials).map({
        case (coefficients2, monomials2) =>
          coefficients2.zip(monomials2).map({
            case (coefficient, monomial) => s"($coefficient * $monomial)"
          }).mkString(" + ")
      }).mkString(" + ")
    }

    def updateCoefficient(degree: Int, index: Int, newCoefficient: Int): Polynomial = {
      val newCoefficients = coefficients.updated(degree, coefficients(degree).updated(index, newCoefficient))
      Polynomial(newCoefficients, monomials)
    }

    // To avoid crashing z3
    def toExpr(solver: Z3Solver): Expr = this.synchronized {
      var result: Expr = solver.mkIntVal(0)
      coefficients.zip(monomials).foreach({
        case (coefficients2, monomials2) =>
          coefficients2.zip(monomials2).foreach({
            case (coefficient, monomial) =>
              if (coefficient != 0)
                result = solver.mkAdd(result, solver.mkMul(solver.mkIntVal(coefficient), monomial.toExpr(solver)))
          })
      })
      result
    }
  }

}
