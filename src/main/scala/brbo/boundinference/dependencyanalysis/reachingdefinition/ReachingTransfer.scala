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
    val definedVariable = n.getTarget.toString
    val newDefinitions = inputDefinitions.filter(value => value.variable != definedVariable) + ReachingValue(Some(n), definedVariable)
    new RegularTransferResult(null, ReachingStore(newDefinitions))
  }
}
