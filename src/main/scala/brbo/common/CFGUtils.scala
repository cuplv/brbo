package brbo.common

import java.io.IOException
import java.util

import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.block.Block
import org.checkerframework.dataflow.cfg.node.{BinaryOperationNode, LocalVariableNode, Node, TernaryExpressionNode, UnaryOperationNode, ValueLiteralNode}
import org.checkerframework.dataflow.cfg.visualize.DOTCFGVisualizer

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

object CFGUtils {
  def printPDF(cfg: ControlFlowGraph): Unit = {
    val viz = new DOTCFGVisualizer
    val args = new java.util.HashMap[java.lang.String, Object]
    args.put("outdir", "./output/cfg")
    // args.put("verbose", true)
    viz.init(args)
    val res = viz.visualize(cfg, cfg.getEntryBlock, null)
    viz.shutdown()

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
      case _ => throw new Exception(s"Get used variables - Node $n (type: ${n.getClass}) is not yet supported")
    }
  }
}
