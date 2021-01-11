package brbo.common

import com.sun.source.tree.{MethodTree, Tree}
import org.checkerframework.dataflow.cfg.ControlFlowGraph

/**
 *
 * @param className     The class name of the method
 * @param methodTree    The method that we wish to analyze
 * @param getLineNumber A function to get line numbers
 * @param cfg           The control flow graph of the method
 */
case class TargetMethod(className: String,
                        methodTree: MethodTree,
                        getLineNumber: Tree => Int,
                        cfg: ControlFlowGraph)
