package bndinfchecker

import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.visualize.DOTCFGVisualizer

object PrintCFG {
  def print(cfg: ControlFlowGraph): Unit ={
    val viz = new DOTCFGVisualizer
    val args = new java.util.HashMap[java.lang.String, Object]
    args.put("outdir", "./output/")
    // args.put("verbose", true)
    viz.init(args)
    val res = viz.visualize(cfg, cfg.getEntryBlock, null)
    viz.shutdown()

    if (res != null) {
      assert(res.get("dotFileName") != null, "@AssumeAssertion(nullness): specification")
      val file = res.get("dotFileName").asInstanceOf[String]
      import java.io.IOException
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
}
