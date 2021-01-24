package brbo.common

import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta, GhostVariable, Resource}
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node, NumericalAdditionNode}

object GhostVariableUtils {
  private val logger = LogManager.getLogger("brbo.common.GhostVariableUtils")

  private val deltaVariablePrefix = "D"
  private val resourceVariablePrefix = "R"
  private val counterVariablePrefix = "C"
  private val deltaVariablePattern = (deltaVariablePrefix + """\d*""").r
  private val resourceVariablePattern = (resourceVariablePrefix + """\d*""").r
  private val counterVariablePattern = (counterVariablePrefix + """\d*""").r

  def generateName(suffix: String, typ: GhostVariable): String = {
    val result = typ match {
      case Resource => s"$resourceVariablePrefix$suffix"
      case Delta => s"$deltaVariablePrefix$suffix"
      case Counter => s"$counterVariablePrefix$suffix"
    }
    assert(isGhostVariable(result, typ))
    result
  }

  def generateName(typ: GhostVariable): String = {
    val r = new scala.util.Random()
    val suffix: Long = {
      val n = r.nextLong()
      if (n < 0) -n else n
    }
    val result = typ match {
      case Resource => s"$resourceVariablePrefix$suffix"
      case Delta => s"$deltaVariablePrefix$suffix"
      case Counter => s"$counterVariablePrefix$suffix"
    }
    assert(isGhostVariable(result, typ), s"Generated name is $result")
    result
  }

  def generateName(avoid: Set[String], typ: GhostVariable): String = {
    var newName = GhostVariableUtils.generateName(typ)
    while(avoid.contains(newName)) {
      newName = GhostVariableUtils.generateName(typ)
    }
    newName
  }

  def isGhostVariable(identifier: String): Boolean = {
    isGhostVariable(identifier, Resource) || isGhostVariable(identifier, Delta) || isGhostVariable(identifier, Counter)
  }

  def isGhostVariable(identifier: String, typ: GhostVariable): Boolean = {
    val pattern = typ match {
      case Resource => resourceVariablePattern
      case Delta => deltaVariablePattern
      case Counter => counterVariablePattern
    }
    identifier match {
      case pattern() => true
      case _ => false
    }
  }

  case class GhostVariableUpdateNode(identifier: String, increment: Node) {
    override def toString: String = s"$identifier = $identifier + $increment"
  }

  case class GhostVariableUpdateTree(identifier: String, increment: Tree) {
    override def toString: String = s"$identifier = $identifier + $increment"
  }

  def extractUpdate(cfgNode: Node, typ: GhostVariable): Option[GhostVariableUpdateNode] = {
    cfgNode match {
      case node: AssignmentNode =>
        // Must be in the form of g = g + e
        val variableName = node.getTarget.toString
        if (isGhostVariable(variableName, typ)) {
          node.getExpression match {
            case rhs: NumericalAdditionNode =>
              if (rhs.getLeftOperand.toString == variableName) Some(GhostVariableUpdateNode(variableName, rhs.getRightOperand))
              else {
                logger.warn(s"Assignment node `$cfgNode` not in the form of `$variableName = $variableName + e`!")
                None
              }
            case _ => None
          }
        }
        else None
      case _ => None
    }
  }

  def extractUpdate(cfgNode: Node, types: Iterable[GhostVariable]): Option[GhostVariableUpdateNode] = {
    types.foldLeft(None: Option[GhostVariableUpdateNode])({
      (acc, typ) =>
        acc match {
          case Some(_) => acc
          case None => extractUpdate(cfgNode, typ)
        }
    })
  }

  def extractUpdate(tree: ExpressionTree, typ: GhostVariable): Option[GhostVariableUpdateTree] = {
    tree match {
      case tree: AssignmentTree =>
        // Must be in the form of g = g + e
        if (isGhostVariable(tree.getVariable.toString, typ)) {
          val ghostVariable = tree.getVariable.toString
          tree.getExpression match {
            case rhs: BinaryTree =>
              if (rhs.getLeftOperand.toString == ghostVariable) Some(GhostVariableUpdateTree(ghostVariable, rhs.getRightOperand))
              else {
                logger.fatal(s"Assignment tree `$tree` not in the form of `$ghostVariable = $ghostVariable + e`!")
                None
              }
            case _ => None
          }
        }
        else None
      case unaryTree: UnaryTree =>
        unaryTree.getKind match {
          case Tree.Kind.POSTFIX_INCREMENT | Tree.Kind.PREFIX_INCREMENT |
               Tree.Kind.POSTFIX_DECREMENT | Tree.Kind.PREFIX_DECREMENT =>
            val variable = unaryTree.getExpression.toString
            if (isGhostVariable(variable, typ))
              logger.fatal(s"Tree `$tree` not in the form of `$variable = $variable + e`!")
            None
          case _ => None
        }
      case _ => None
    }
  }

  def isUpdate(tree: ExpressionTree): Boolean = {
    extractUpdate(tree, Resource).isDefined || extractUpdate(tree, Delta).isDefined || extractUpdate(tree, Counter).isDefined
  }

  def extractReset(cfgNode: Node): Option[String] = {
    cfgNode match {
      case node: AssignmentNode =>
        val variableName = node.getTarget.toString
        if (isGhostVariable(variableName, Delta)) {
          // Must be in the form of `D = 0`
          if (node.getExpression.toString == "0") Some(variableName)
          else None
        }
        else None
      case _ => None
    }
  }

  def extractReset(tree: ExpressionTree, typ: GhostVariable): Option[String] = {
    tree match {
      case tree: AssignmentTree =>
        if (isGhostVariable(tree.getVariable.toString, typ)) {
          val ghostVariable = tree.getVariable.toString
          if (tree.getExpression.toString == "0") Some(ghostVariable)
          else None
        }
        else None
      case _ => None
    }
  }

  def generateDeltaVariablePrime(identifier: String): String = {
    assert(GhostVariableUtils.isGhostVariable(identifier, Delta))
    s"${identifier}p"
  }

  @deprecated
  def generateDeltaVariableDoublePrime(identifier: String): String = {
    assert(GhostVariableUtils.isGhostVariable(identifier, Delta))
    s"$identifier\'\'"
  }

  def extractDeltaPrime(tree: ExpressionTree): Option[String] = {
    tree match {
      case tree: AssignmentTree =>
        val identifier = tree.getVariable.toString
        if (isGhostVariable(identifier.dropRight(1), Delta)) {
          // TODO:
          Some(identifier)
        }
        else None
      case _ => None
    }
  }

  def isReset(tree: ExpressionTree): Boolean = {
    extractReset(tree, Resource).isDefined || extractReset(tree, Delta).isDefined || extractReset(tree, Counter).isDefined
  }

  object GhostVariable extends Enumeration {
    type GhostVariable = Value
    val Resource, Delta, Counter = Value
  }

  object UpdateOrReset extends Enumeration {
    type UpdateOrReset = Value
    val Update, Reset = Value
  }

}
