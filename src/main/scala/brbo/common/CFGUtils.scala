package brbo.common

import java.io.IOException
import java.util

import brbo.verification.dependency.BrboNode
import com.ibm.wala.util.graph.NumberedGraph
import com.ibm.wala.util.graph.dominators.Dominators
import com.ibm.wala.util.graph.impl.DelegatingNumberedGraph
import com.sun.source.tree.{ExpressionStatementTree, StatementTree}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.block.Block
import org.checkerframework.dataflow.cfg.node._
import org.checkerframework.dataflow.cfg.visualize.DOTCFGVisualizer

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

object CFGUtils {
  private val logger = LogManager.getLogger("brbo.common.CFGUtils")

  def printPDF(cfg: ControlFlowGraph): Unit = {
    val visualizer = new DOTCFGVisualizer
    val args = new java.util.HashMap[java.lang.String, Object]
    args.put("outdir", "./output/cfg".asInstanceOf[Object])
    // args.put("verbose", "true".asInstanceOf[Object])
    visualizer.init(args)
    val res = visualizer.visualize(cfg, cfg.getEntryBlock, null)
    visualizer.shutdown()

    if (res != null) {
      assert(res.get("dotFileName") != null, "@AssumeAssertion(nullness): specification")
      val file = res.get("dotFileName").asInstanceOf[String]
      try {
        val command = "dot -Tpdf \"" + file + "\" -o \"" + file + ".pdf\""
        val child = Runtime.getRuntime.exec(Array[String]("/bin/sh", "-c", command))
        child.waitFor
      } catch {
        case e@(_: InterruptedException | _: IOException) =>
          e.printStackTrace()
          System.exit(1)
      }
    }
  }

  def existsPath(from: Block, to: Block): Boolean = {
    val visited = new util.HashSet[Block]
    val worklist = new util.ArrayDeque[Block]
    var cur = from
    visited.add(cur)

    while (cur != null) {
      val succs = cur.getSuccessors
      succs.asScala.foreach({
        b =>
          if (!visited.contains(b)) {
            visited.add(b)
            worklist.add(b)
          }
      })
      cur = worklist.poll
    }

    visited.contains(to)
  }

  def getUsedVariables(n: Node): Set[String] = {
    n match {
      case n: LocalVariableNode => HashSet(n.getName)
      case n: UnaryOperationNode => getUsedVariables(n.getOperand)
      case n: TernaryExpressionNode =>
        getUsedVariables(n.getConditionOperand) ++ getUsedVariables(n.getThenOperand) ++ getUsedVariables(n.getElseOperand)
      case n: BinaryOperationNode =>
        getUsedVariables(n.getLeftOperand) ++ getUsedVariables(n.getRightOperand)
      case _: ValueLiteralNode => new HashSet[String]
      case _ => throw new Exception(s"Get used variables - Node `$n` (type: `${n.getClass}`) is not yet supported")
    }
  }

  def getAllNodes(blocks: Iterable[Block]): Set[Node] = {
    blocks.flatMap(block => block.getNodes.asScala).toSet
  }

  /**
   *
   * @param tree An expression statement tree
   * @param cfg  A control flow graph
   * @return The node that corresponds to an expression statement tree
   *         Check CFGTranslationPhaseOne to see which trees may serve as a key
   */
  def getNodesCorrespondingToExpressionStatementTree(tree: StatementTree, cfg: ControlFlowGraph): Node = {
    val nodes = cfg.getNodesCorrespondingToTree(tree.asInstanceOf[ExpressionStatementTree].getExpression).asScala
    assert(nodes.size == 1)
    nodes.head
  }

  def deepCopyGraph(cfg: ControlFlowGraph, transpose: Boolean): (NumberedGraph[BrboNode], BrboNode) = {
    var map = new HashMap[Block, BrboNode]

    def getNode(block: Block): BrboNode = {
      map.get(block) match {
        case Some(node) => node
        case None =>
          val node = BrboNode(block)
          map = map + (block -> node)
          node
      }
    }

    val graph = new DelegatingNumberedGraph[BrboNode]()

    var visited = new HashSet[Block]
    val stack = new java.util.Stack[Block]

    stack.push(cfg.getEntryBlock)
    while (!stack.empty()) {
      val top: Block = stack.pop()
      val topNode: BrboNode = getNode(top)

      if (!visited.contains(top)) {
        visited = visited + top
        // Add the node upon the 1st visit
        graph.addNode(topNode)
      }

      top.getSuccessors.asScala.foreach({
        block =>
          if (!visited.contains(block))
            stack.push(block)

          // Add the reversed edge
          val from = topNode
          val to = getNode(block)
          if (transpose) graph.addEdge(to, from)
          else graph.addEdge(from, to)
      })
    }

    val root = if (transpose) cfg.getRegularExitBlock else cfg.getEntryBlock
    (graph, map(root))
  }

  def findAllBackEdges[T](graph: NumberedGraph[T], root: T): Set[(T, T)] = {
    var visited = new HashSet[T]
    var result = new HashSet[(T, T)]
    val stack = new java.util.Stack[T]

    stack.push(root)
    while (!stack.empty()) {
      val top = stack.pop()

      if (!visited.contains(top))
        visited = visited + top

      graph.getSuccNodes(top).asScala.foreach({
        successor =>
          if (!visited.contains(successor)) stack.push(successor)
          else result = result + ((top, successor))
      })
    }
    result
  }

  def entryOfMinimalEnclosingLoop(node: Node, targetMethod: TargetMethod): Option[Node] = {
    val graph = targetMethod.numberedGraph
    val root = targetMethod.rootOfNumberedGraph
    val backEdges: Set[(BrboNode, BrboNode)] = findAllBackEdges(graph, root)
    val dominators = Dominators.make(graph, root)

    graph.iterator().asScala.find({
      brboNode: BrboNode =>
        val nodes = brboNode.block.getNodes
        if (nodes == null) false
        else nodes.contains(node)
    }) match {
      case Some(brboNode) =>
        dominators.dominators(brboNode).asScala.find({
          dominator => backEdges.exists({ case (_, to) => to == dominator })
        }) match {
          case Some(brboNode2) => Some(brboNode2.block.getNodes.asScala.head)
          case None =>
            logger.debug(s"No back edge to any dominator of block ${brboNode.block}")
            None
        }
      case None => throw new Exception(s"Node $node should exist in the CFG of method ${targetMethod.methodTree.getName}")
    }
  }
}
