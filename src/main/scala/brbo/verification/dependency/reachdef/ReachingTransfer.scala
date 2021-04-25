package brbo.verification.dependency.reachdef

import org.apache.logging.log4j.{LogManager, Logger}
import org.checkerframework.dataflow.analysis.{ForwardTransferFunction, RegularTransferResult, TransferInput, TransferResult}
import org.checkerframework.dataflow.cfg.UnderlyingAST
import org.checkerframework.dataflow.cfg.node._

import scala.collection.JavaConverters._

class ReachingTransfer
  extends AbstractNodeVisitor[TransferResult[ReachingValue, ReachingStore], TransferInput[ReachingValue, ReachingStore]]
    with ForwardTransferFunction[ReachingValue, ReachingStore] {
  private val logger: Logger = LogManager.getLogger(classOf[ReachingTransfer])

  override def initialStore(underlyingAST: UnderlyingAST, parameters: java.util.List[LocalVariableNode]): ReachingStore = {
    ReachingStore(parameters.asScala.map(n => ReachingValue(None, n.getName)).toSet)
  }

  override def visitNode(n: Node, p: TransferInput[ReachingValue, ReachingStore]): TransferResult[ReachingValue, ReachingStore] = {
    new RegularTransferResult(null, p.getRegularStore)
  }

  // Ignore variable declaration node, because it contains no initializer!
  // E.g., `int x = 0` is represented as a declaration node `int x` and an assignment node `x = 0`
  /*override def visitVariableDeclaration(n: VariableDeclarationNode, p: TransferInput[ReachingValue, ReachingStore]): TransferResult[ReachingValue, ReachingStore] = {
    val inputDefinitions = super.visitVariableDeclaration(n, p).getRegularStore.definitions
    val definedVariable = n.getName
    val newDefinitions = {
      val killed = inputDefinitions.filter(value => value.variable == definedVariable)
      val generated = ReachingValue(Some(n), definedVariable)
      val result = inputDefinitions + generated
      logger.error(s"[Variable declaration] Node `$n`\nInput: $inputDefinitions\nKilled: $killed\nGenerated: $generated\nResult: $result")
      result
    }
    new RegularTransferResult(null, ReachingStore(newDefinitions))
  }*/

  override def visitAssignment(n: AssignmentNode, p: TransferInput[ReachingValue, ReachingStore]): TransferResult[ReachingValue, ReachingStore] = {
    val inputDefinitions = super.visitAssignment(n, p).getRegularStore.definitions
    val definedVariable = n.getTarget.toString
    val newDefinitions = {
      val killed = inputDefinitions.filter(value => value.variable == definedVariable)
      val generated = ReachingValue(Some(n), definedVariable)
      val result = inputDefinitions -- killed + generated
      logger.trace(s"[Assignment] Node `$n`\nInput: $inputDefinitions\nKilled: $killed\nGenerated: $generated\nResult: $result")
      result
    }
    new RegularTransferResult(null, ReachingStore(newDefinitions))
  }
}
