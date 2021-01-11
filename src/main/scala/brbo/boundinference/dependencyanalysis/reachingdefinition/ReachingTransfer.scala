package brbo.boundinference.dependencyanalysis.reachingdefinition

import org.checkerframework.dataflow.analysis.{ForwardTransferFunction, RegularTransferResult, TransferInput, TransferResult}
import org.checkerframework.dataflow.cfg.UnderlyingAST
import org.checkerframework.dataflow.cfg.node._

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

class ReachingTransfer
  extends AbstractNodeVisitor[TransferResult[ReachingValue, ReachingStore], TransferInput[ReachingValue, ReachingStore]]
    with ForwardTransferFunction[ReachingValue, ReachingStore] {
  override def initialStore(underlyingAST: UnderlyingAST, parameters: java.util.List[LocalVariableNode]): ReachingStore = {
    ReachingStore(parameters.asScala.map(n => ReachingValue(None, n.getName)).toSet)
  }

  override def visitNode(n: Node, p: TransferInput[ReachingValue, ReachingStore]): TransferResult[ReachingValue, ReachingStore] = {
    new RegularTransferResult(null, p.getRegularStore)
  }

  override def visitVariableDeclaration(n: VariableDeclarationNode, p: TransferInput[ReachingValue, ReachingStore]): TransferResult[ReachingValue, ReachingStore] = {
    val inputDefinitions = super.visitVariableDeclaration(n, p).getRegularStore.definitions
    val newDefinitions = inputDefinitions + ReachingValue(Some(n), n.getName)
    new RegularTransferResult(null, ReachingStore(newDefinitions))
  }

  override def visitAssignment(n: AssignmentNode, p: TransferInput[ReachingValue, ReachingStore]): TransferResult[ReachingValue, ReachingStore] = {
    val inputDefinitions = super.visitAssignment(n, p).getRegularStore.definitions
    val usedVariables = getUsedVariables(n.getExpression)
    val newDefinitions =
      inputDefinitions.filterNot({
        value => usedVariables.contains(value.variable)
      }) + ReachingValue(Some(n), n.getTarget.toString)
    new RegularTransferResult(null, ReachingStore(newDefinitions))
  }

  private def getUsedVariables(n: Node): Set[String] = {
    n match {
      case n: LocalVariableNode => HashSet(n.getName)
      case n: UnaryOperationNode => getUsedVariables(n.getOperand)
      case n: TernaryExpressionNode =>
        getUsedVariables(n.getConditionOperand) ++ getUsedVariables(n.getThenOperand) ++ getUsedVariables(n.getElseOperand)
      case n: BinaryOperationNode =>
        getUsedVariables(n.getLeftOperand) ++ getUsedVariables(n.getRightOperand)
      case _: ValueLiteralNode => new HashSet[String]
      case _ => throw new Exception(s"Reaching definition analysis - Node $n (type: ${n.getClass}) is not yet supported")
    }
  }
}
