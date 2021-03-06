package brbo.common

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT, VOID}
import brbo.common.cfg.CFGUtils.deepCopyGraph
import brbo.common.cfg.UniqueNode
import brbo.verification.dependency.{BrboNode, ControlDependency, ReachingDefinition}
import com.ibm.wala.util.graph.NumberedGraph
import com.sun.source.tree.{MethodTree, StatementTree, Tree}
import com.sun.source.util.TreePath
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.block.Block

import scala.collection.immutable.HashMap

/**
 *
 * @param fullQualifiedClassName The class name of the method
 * @param methodTree             The method that we wish to analyze
 * @param getLineNumber          A function to get line numbers
 * @param cfg                    The control flow graph of the method
 */
case class TargetMethod(fullQualifiedClassName: String,
                        methodTree: MethodTree,
                        cfg: ControlFlowGraph,
                        getLineNumber: Tree => Int,
                        getPath: Tree => TreePath,
                        sourceCode: String) {
  private val logger = LogManager.getLogger(classOf[TargetMethod])

  TreeUtils.acceptableTree(methodTree.getBody)

  val inputVariables: Map[String, BrboType] = TreeUtils.getAllInputVariables(methodTree)
  logger.trace(s"[Method `${methodTree.getName}`] Input variables: `$inputVariables`")

  val localVariables: Map[String, BrboType] =
    if (methodTree.getBody == null) new HashMap[String, BrboType]
    else TreeUtils.getAllDeclaredVariables(methodTree.getBody)
  logger.trace(s"[Method `${methodTree.getName}`] Local variables: `$localVariables`")

  val returnType: BrboType = methodTree.getReturnType.toString match {
    case "int" => INT
    case "boolean" => BOOL
    case "void" => VOID
    case _ => throw new Exception(s"Unexpected return type: ${methodTree.getReturnType} (Kind: ${methodTree.getReturnType.getKind})")
  }

  val (numberedGraph: NumberedGraph[BrboNode], rootOfNumberedGraph: BrboNode) = deepCopyGraph(cfg, transpose = false)

  val className: String = {
    // We expect input method's class name to be a fully qualified class name (e.g., `x.y.z.OutputHandler`)
    val index = fullQualifiedClassName.lastIndexOf(".")
    val newClassName = fullQualifiedClassName.substring(index + 1)
    logger.trace(s"New class name: `$newClassName`")
    newClassName
  }

  val packageName: Option[String] = {
    fullQualifiedClassName.lastIndexOf(".") match {
      case -1 => None
      case index =>
        val packageName = fullQualifiedClassName.substring(0, index)
        logger.trace(s"Package name: `$packageName`")
        Some(packageName)
    }
  }

  val sortedCommands: List[StatementTree] = TreeUtils.collectCommands(methodTree.getBody).toList.sortWith({ case (c1, c2) => c1.toString < c2.toString })
  val commandsNodesMap: Map[StatementTree, Set[UniqueNode]] = {
    sortedCommands.foldLeft(new HashMap[StatementTree, Set[UniqueNode]])({
      (acc, command) => acc + (command -> TreeUtils.getNodesCorrespondingToCommand(cfg, command))
    })
  }

  def reachingDefinitions: ReachingDefinition = ReachingDefinition.run(this)

  def controlDependency: Map[Block, Set[Block]] = ControlDependency.computeReverseControlDependency(this)
}
