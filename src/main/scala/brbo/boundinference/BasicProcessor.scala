package brbo.boundinference

import brbo.boundinference.FileFormat.{C_FORMAT, FileFormat, JAVA_FORMAT}
import brbo.common.Instrument
import brbo.common.Instrument.InstrumentMode.InstrumentMode
import brbo.common.Instrument.{AtomicStatementInstrumentation, InstrumentResult}
import com.sun.source.tree.{ClassTree, CompilationUnitTree, MethodTree, Tree}
import com.sun.source.util.{SourcePositions, TreePathScanner, Trees}
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

  protected val indent: Int = Instrument.INDENT

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

  protected def getEnclosingClass(node: MethodTree): Option[ClassTree] = {
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

  def getCompilationUnitName: String = compilationUnitName.get

  def getSourceCode: String = sourceCode.get

  def testInstrumentation(atomicStatementInstrumentation: AtomicStatementInstrumentation, instrumentMode: InstrumentMode): Map[MethodTree, InstrumentResult] = {
    getMethods.map({
      case (methodTree, cfg) =>
        val instrumentedSourceCode =
          Instrument.substituteAtomicStatements(methodTree.getBody, atomicStatementInstrumentation, 0, cfg, getLineNumber, instrumentMode)
        (methodTree, instrumentedSourceCode)
    })
  }

  def assumeOneClassOneMethod(): Unit = {
    assert(getClasses.size == 1, s"We should analyze exactly one class. Instead, we have `$getClasses`")
    assert(getMethods.size == 1, s"We should analyze exactly one class. Instead, we have `$getMethods`")
  }

  def insertDeclarationAtEntry(): String = {
    ???
  }

  def replaceMethodBodyAndGenerateSourceCode(methodTree: MethodTree,
                                             className: String,
                                             newMethodBody: String,
                                             fileFormat: FileFormat): String = {
    assumeOneClassOneMethod()

    val cFilePrefix =
      """extern void __VERIFIER_error() __attribute__((noreturn));
        |extern void __VERIFIER_assume (int);
        |extern int __VERIFIER_nondet_int ();
        |#define static_assert __VERIFIER_assert
        |#define assume __VERIFIER_assume
        |#define LARGE_INT 1000000
        |void __VERIFIER_assert(int cond) {
        |  if (!(cond)) {
        |    ERROR: __VERIFIER_error();
        |  }
        |  return;
        |}
        |void assert(int cond) {
        |  if (!(cond)) {
        |    ERROR: __VERIFIER_error();
        |  }
        |  return;
        |}""".stripMargin

    val methodSignature = {
      // TODO: A very hacky way to get the first line of a method definition
      val lines = methodTree.toString.split("\n")
      // https://stackoverflow.com/a/39259747
      // lines(1).replace(" {", "")
      var firstLine = lines(1)
      firstLine = firstLine.replaceAll("\\n", "")
      firstLine = firstLine.replaceAll("\\r", "")
      assert(firstLine.endsWith(" {"))
      firstLine.substring(0, firstLine.length - 2)
    }
    fileFormat match {
      case JAVA_FORMAT =>
        val spaces = " " * indent
        s"class $className {\n$spaces$methodSignature\n$newMethodBody\n}"
      case C_FORMAT =>
        val replaceMethodSignature = {
          // ICRA requires there exists a method named as `main`
          val startIndex = methodSignature.indexOf(" ")
          val endIndex = methodSignature.indexOf("(")
          s"${methodSignature.substring(0, startIndex + 1)}main${methodSignature.substring(endIndex)}"
        }
        val replaceAssertOne = newMethodBody.replace("assert(true)", "assert(1)")
        s"$cFilePrefix\n$replaceMethodSignature\n$replaceAssertOne"
    }
  }
}
