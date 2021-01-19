package brbo.verification

import brbo.common.{InstrumentUtils, JavacUtils, TargetMethod}
import com.sun.source.tree.{ClassTree, CompilationUnitTree, MethodTree, Tree}
import com.sun.source.util.{SourcePositions, TreePath, TreePathScanner, Trees}
import javax.annotation.processing.SupportedAnnotationTypes
import javax.lang.model.SourceVersion
import org.apache.commons.io.FilenameUtils
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.UnderlyingAST.CFGMethod
import org.checkerframework.dataflow.cfg.builder.CFGBuilder
import org.checkerframework.javacutil.BasicTypeProcessor

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

@SupportedAnnotationTypes(Array("*"))
class BasicProcessor extends BasicTypeProcessor {
  private val logger = LogManager.getLogger(classOf[BasicProcessor])

  private var sourceCode: Option[String] = None
  private var compilationUnitName: Option[String] = None

  private var rootTree: Option[CompilationUnitTree] = None
  private var trees: Option[Trees] = None
  private var positions: Option[SourcePositions] = None

  private var classes = new HashMap[ClassTree, Set[MethodTree]]
  private var methods = new HashMap[MethodTree, ControlFlowGraph]

  protected val indent: Int = InstrumentUtils.INDENT

  def runAnalysis(): Unit = {}

  override protected def createTreePathScanner(root: CompilationUnitTree): TreePathScanner[_, _] = {
    rootTree = Some(root)
    sourceCode = Some(root.getSourceFile.getCharContent(false).toString)
    compilationUnitName = Some(FilenameUtils.getBaseName(root.getSourceFile.getName))

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

    super.typeProcessingOver()

    // Stop execution by throwing an exception. This makes sure that compilation
    // does not proceed, and thus the AST is not modified by further phases of
    // the compilation (and we save the work to do the compilation).
    throw new EarlyStopException("AbstractProcessor: Stop execution by throwing an exception")
  }

  override def getSupportedSourceVersion: SourceVersion = SourceVersion.latestSupported

  def getEnclosingClass(node: MethodTree): Option[ClassTree] = {
    classes.find({
      case (_, methods) => methods.contains(node)
    }) match {
      case Some(candidate) => Some(candidate._1)
      case None => None
    }
  }

  def getLineNumber(node: Tree): Int = {
    def getLineNumber(node: Tree, positions: SourcePositions, root: CompilationUnitTree): Long = {
      root.getLineMap.getLineNumber(positions.getStartPosition(root, node))
    }

    getLineNumber(node, positions.get, rootTree.get).toInt
  }

  private def getFileName: String = rootTree.get.getSourceFile.getName

  def getClasses: HashMap[ClassTree, Set[MethodTree]] = classes

  def getMethods: HashMap[MethodTree, ControlFlowGraph] = methods

  def getCompilationUnitName: String = compilationUnitName.get

  def getSourceCode: String = sourceCode.get

  def assumeOneClassOneMethod(): Unit = {
    assert(getClasses.size == 1, s"We should analyze exactly one class. Instead, we have `$getClasses`")
    assert(getMethods.size == 1, s"We should analyze exactly one method. Instead, we have `$getMethods`")
  }

  def getPath(tree: Tree): TreePath = trees.get.getPath(rootTree.get, tree)
}

object BasicProcessor {
  private val logger = LogManager.getLogger("brbo.verification.BasicProcessor")

  private def run(className: String, sourceFileContents: String): BasicProcessor = {
    val basicProcessor = new BasicProcessor
    try {
      JavacUtils.runProcessor(className, sourceFileContents, basicProcessor)
    }
    catch {
      case e: Exception =>
        logger.fatal(s"Compilation error for program:\n$sourceFileContents")
        throw e
    }
    basicProcessor
  }

  /**
   *
   * @param className          The class name
   * @param sourceFileContents Java source code that contains a class, which defines exactly 1 method
   * @return The method in the Java source code
   */
  def getTargetMethod(className: String, sourceFileContents: String): TargetMethod = {
    val processor = run(className, sourceFileContents)
    processor.assumeOneClassOneMethod()
    TargetMethod(className, processor.getMethods.head._1, processor.getMethods.head._2, processor.getLineNumber, processor.getPath)
  }
}