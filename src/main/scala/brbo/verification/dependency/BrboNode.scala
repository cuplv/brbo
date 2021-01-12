package brbo.verification.dependency

import com.ibm.wala.util.graph.INodeWithNumberedEdges
import com.ibm.wala.util.graph.impl.NodeWithNumber
import com.ibm.wala.util.intset.{BimodalMutableIntSet, IntSet}
import org.checkerframework.dataflow.cfg.block.Block

case class BrboNode(block: Block) extends NodeWithNumber with INodeWithNumberedEdges {
  private val predNumbers = new BimodalMutableIntSet()
  private val succNumbers = new BimodalMutableIntSet()

  override def getGraphNodeId: Int = block.getUid.toInt

  override def addPred(n: Int): Unit = predNumbers.add(n)

  override def addSucc(n: Int): Unit = succNumbers.add(n)

  override def getPredNumbers: IntSet = predNumbers

  override def getSuccNumbers: IntSet = succNumbers

  override def removeIncomingEdges(): Unit = ???

  override def removeOutgoingEdges(): Unit = ???

  override def removeAllIncidentEdges(): Unit = ???

  override def toString: String = block.toString
}
