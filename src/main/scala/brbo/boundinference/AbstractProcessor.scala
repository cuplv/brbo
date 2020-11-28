package brbo.boundinference

import com.sun.source.tree.{ClassTree, CompilationUnitTree, MethodTree, Tree}
import com.sun.source.util.{SourcePositions, TreePathScanner, Trees}
import javax.annotation.processing.SupportedAnnotationTypes
import javax.lang.model.SourceVersion
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.UnderlyingAST.CFGMethod
import org.checkerframework.dataflow.cfg.builder.CFGBuilder
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node}
import org.checkerframework.javacutil.BasicTypeProcessor

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

@SupportedAnnotationTypes(Array("*"))
abstract class AbstractProcessor extends BasicTypeProcessor {
  private val logger = LogManager.getLogger(classOf[AbstractProcessor])

  private var rootTree: Option[CompilationUnitTree] = None
  private var trees: Option[Trees] = None
  private var positions: Option[SourcePositions] = None

  private var classes = new HashMap[ClassTree, Set[MethodTree]]
  private var methods = new HashMap[MethodTree, ControlFlowGraph]

  protected var result: Option[Any] = None

  def runAnalysis(): Unit

  def getResult: Option[Any] = result

  override protected def createTreePathScanner(root: CompilationUnitTree): TreePathScanner[_, _] = {
    rootTree = Some(root)
    new TreePathScanner[Void, Void]() {
      override def visitClass(node: ClassTree, p: Void): Void = {
        val members = node.getMembers.asScala.toSet
        val methods: Set[MethodTree] =
          members.filter({
            case _: MethodTree => true
            case _ => false
          }).map(t => t.asInstanceOf[MethodTree])
        classes = classes + (node -> methods)
        super.visitClass(node, p)
      }

      override def visitMethod(node: MethodTree, p: Void): Void = {
        if (node.getBody == null || node.getName.toString == "<init>")
          return null
        logger.debug(s"Visiting method `${node.getName}` in file `$getFileName`")

        val underlyingAST = new CFGMethod(node, getEnclosingClass(node).get)
        val cfg: ControlFlowGraph = CFGBuilder.build(root, underlyingAST, false, true, processingEnv)

        // CFGUtils.printPDF(cfg)

        methods = methods + (node -> cfg)
        null
      }
    }
  }

  override def typeProcessingOver(): Unit = {
    val log = getCompilerLog
    if (log.nerrors > 0) {
      logger.error(s"Compilation error in file `$getFileName`: ${log.toString}")
      return
    }

    trees = Some(Trees.instance(processingEnv))
    positions = Some(trees.get.getSourcePositions)

    runAnalysis()

    super.typeProcessingOver()

    // Stop execution by throwing an exception. This makes sure that compilation
    // does not proceed, and thus the AST is not modified by further phases of
    // the compilation (and we save the work to do the compilation).
    throw new EarlyStopException("AbstractProcessor: Stop execution by throwing an exception")
  }

  override def getSupportedSourceVersion: SourceVersion = SourceVersion.latestSupported

  private def getEnclosingClass(node: MethodTree): Option[ClassTree] = {
    classes.find({
      case (_, methods) => methods.contains(node)
    }) match {
      case Some(candidate) => Some(candidate._1)
      case None => None
    }
  }

  protected def getLineNumber(node: Tree): Int = {
    def getLineNumber(node: Tree, positions: SourcePositions, root: CompilationUnitTree): Long = {
      root.getLineMap.getLineNumber(positions.getStartPosition(root, node))
    }

    getLineNumber(node, positions.get, rootTree.get).toInt
  }

  private def getFileName: String = rootTree.get.getSourceFile.getName

  def getClasses: HashMap[ClassTree, Set[MethodTree]] = classes

  def getMethods: HashMap[MethodTree, ControlFlowGraph] = methods

  case class AssignmentToDeltaVariable(cfgNode: Node, deltaVariable: String) {
    assert(cfgNode.isInstanceOf[AssignmentNode])

    override def toString: String = s"delta variable $deltaVariable in stmt $cfgNode at line ${getLineNumber(cfgNode.getTree)}"
  }

}
