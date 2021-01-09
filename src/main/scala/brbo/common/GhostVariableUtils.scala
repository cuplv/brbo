package brbo.common

import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta, GhostVariable, Resource}
import com.sun.source.tree.{AssignmentTree, BinaryTree, Tree}
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

  def generateGhostVariable(suffix: String, typ: GhostVariable): String = {
    val result = typ match {
      case Resource => s"$resourceVariablePrefix$suffix"
      case Delta => s"$deltaVariablePrefix$suffix"
      case Counter => s"$counterVariablePrefix$suffix"
    }
    assert(isGhostVariable(result, typ))
    result
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

  case class GhostVariableUpdateNode(identifier: String, update: Node) {
    override def toString: String = s"$identifier = $identifier + $update"
  }

  case class GhostVariableUpdateTree(identifier: String, update: Tree) {
    override def toString: String = s"$identifier = $identifier + $update"
  }

  def extractDeltaVariableReset(cfgNode: Node): Option[String] = {
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

  def extractGhostVariableFromAssignment(cfgNode: Node, typ: GhostVariable): Option[GhostVariableUpdateNode] = {
    cfgNode match {
      case node: AssignmentNode =>
        // Must be in the form of g = g + e
        val variableName = node.getTarget.toString
        if (isGhostVariable(variableName, typ)) {
          node.getExpression match {
            case rhs: NumericalAdditionNode =>
              if (rhs.getLeftOperand.toString == variableName) Some(GhostVariableUpdateNode(variableName, rhs.getRightOperand))
              else {
                logger.warn(s"Assignment to ghost variable `$variableName` is not in the form of `$variableName = $variableName + e`!")
                None
              }
            case _ => None
          }
        }
        else None
      case _ => None
    }
  }

  def extractGhostVariableFromAssignment(cfgNode: Node, types: Iterable[GhostVariable]): Option[GhostVariableUpdateNode] = {
    types.foldLeft(None: Option[GhostVariableUpdateNode])({
      (acc, typ) =>
        acc match {
          case Some(_) => acc
          case None => extractGhostVariableFromAssignment(cfgNode, typ)
        }
    })
  }

  def extractGhostVariableFromAssignment(tree: Tree, typ: GhostVariable): Option[GhostVariableUpdateTree] = {
    tree match {
      case tree: AssignmentTree =>
        // Must be in the form of g = g + e
        if (isGhostVariable(tree.getVariable.toString, typ)) {
          val ghostVariable = tree.getVariable.toString
          tree.getExpression match {
            case rhs: BinaryTree =>
              if (rhs.getLeftOperand.toString == ghostVariable) Some(GhostVariableUpdateTree(ghostVariable, rhs.getRightOperand))
              else {
                logger.warn(s"Assignment to ghost variable `$ghostVariable` is not in the form of `$ghostVariable = $ghostVariable + e`!")
                None
              }
            case _ => None
          }
        }
        else None
      case _ => None
    }
  }

  object GhostVariable extends Enumeration {
    type GhostVariable = Value
    val Resource, Delta, Counter = Value
  }

}
