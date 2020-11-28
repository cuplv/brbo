package brbo.boundinference

import com.sun.source.tree.{ClassTree, CompilationUnitTree, MethodTree, Tree}
import com.sun.source.util.{SourcePositions, TreePathScanner, Trees}
import javax.annotation.processing.SupportedAnnotationTypes
import javax.lang.model.SourceVersion
import org.apache.logging.log4j.LogManager
import org.checkerframework.javacutil.BasicTypeProcessor

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}


@SupportedAnnotationTypes(Array("*"))
class BoundInferenceProcessor extends BasicTypeProcessor {
  private val logger = LogManager.getLogger(classOf[BoundInferenceProcessor])

  private var rootTree: Option[CompilationUnitTree] = None
  private var trees: Option[Trees] = None
  private var positions: Option[SourcePositions] = None

  private var classes = new HashMap[ClassTree, Set[MethodTree]]
  private var methods = new HashSet[MethodTree]

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
          logger.debug(s"Visiting method `${node.getName}` in file `$getFileName`")
        methods = methods + node
        // Stop execution by throwing an exception. This
        // makes sure that compilation does not proceed, and
        // thus the AST is not modified by further phases of
        // the compilation (and we save the work to do the
        // compilation).
        throw new RuntimeException("BoundInferenceProcessor: Stop execution by throwing an exception")
      }
    }
  }

  override def getSupportedSourceVersion: SourceVersion = SourceVersion.latestSupported

  override def typeProcessingOver(): Unit = {
    trees = Some(Trees.instance(processingEnv))
    positions = Some(trees.get.getSourcePositions)

    super.typeProcessingOver()
  }

  private def getEnclosingClass(node: MethodTree): Option[ClassTree] = {
    classes.find({
      case (_, methods) => methods.contains(node)
    }) match {
      case Some(candidate) => Some(candidate._1)
      case None => None
    }
  }

  private def getLineNumber(node: Tree): Int = {
    def getLineNumber(node: Tree, positions: SourcePositions, root: CompilationUnitTree): Long = {
      root.getLineMap.getLineNumber(positions.getStartPosition(root, node))
    }

    getLineNumber(node, positions.get, rootTree.get).toInt
  }

  private def getFileName: String = rootTree.get.getSourceFile.getName
}
