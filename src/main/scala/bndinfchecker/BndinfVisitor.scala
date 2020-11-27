package bndinfchecker

import com.sun.source.tree.{ClassTree, CompilationUnitTree, MethodTree, Tree}
import com.sun.source.util.SourcePositions
import javax.lang.model.`type`.TypeKind
import org.apache.logging.log4j.LogManager
import org.checkerframework.common.basetype.{BaseAnnotatedTypeFactory, BaseTypeChecker, BaseTypeVisitor}
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.UnderlyingAST.CFGMethod
import org.checkerframework.dataflow.cfg.block.Block.BlockType
import org.checkerframework.dataflow.cfg.block.{ConditionalBlock, ExceptionBlock, RegularBlock, SpecialBlock}
import org.checkerframework.dataflow.cfg.builder.CFGBuilder
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node, NumericalAdditionNode}
import org.checkerframework.javacutil.TreeUtils

import scala.collection.JavaConverters._

class BndinfVisitor(checker: BaseTypeChecker) extends BaseTypeVisitor[BaseAnnotatedTypeFactory](checker) {
  private val logger = LogManager.getLogger(classOf[BndinfVisitor])
  private val deltaVariablePattern = """D\d*""".r

  override def visitMethod(node: MethodTree, p: Void): Void = {
    if (node.getBody == null || node.getName.toString == "<init>")
      return super.visitMethod(node, p)
    logger.debug(s"Visiting method: ${node.getName}")

    val underlyingAST = new CFGMethod(node, getEnclosingClass(node))
    val cfg: ControlFlowGraph = CFGBuilder.build(root, underlyingAST, false, true, this.checker.getProcessingEnvironment)

    CFGUtils.printPDF(cfg)

    val allBlocks = cfg.getAllBlocks.asScala
    cfg.getAllNodes.asScala.foldLeft(List[AssignmentToDeltaVariable]())({
      (acc, cfgNode) =>
        cfgNode match {
          case cfgNode: AssignmentNode => extractDeltaVariableFromAssignment(cfgNode) match {
            case Some(deltaVariable) =>
              // We avoid using hash sets to store results, because we may have different
              // assignments that will be determined as a same assignment in a hash set
              AssignmentToDeltaVariable(cfgNode, deltaVariable) :: acc
            case _ => acc
          }
          case _ => acc
        }
    }).foreach({
      assignmentToDeltaVariable =>
        logger.debug(s"Placing accumulation contexts for delta variable ${assignmentToDeltaVariable.deltaVariable} " +
          s"in stmt ${assignmentToDeltaVariable.cfgNode} " +
          s"at line ${getLineNumber(assignmentToDeltaVariable.cfgNode.getTree)} " +
          s"in file $getFileName")

        val thisBlock = assignmentToDeltaVariable.cfgNode.getBlock
        allBlocks.filter({
          block => (block != thisBlock) && CFGUtils.existsPath(block, thisBlock) && CFGUtils.existsPath(thisBlock, block)
        }).foreach({
          block =>
            block match {
              case _: ConditionalBlock => // Do nothing
              case _: ExceptionBlock => // Do nothing
              case block: RegularBlock =>
                if (block.getSuccessor.getType == BlockType.CONDITIONAL_BLOCK) {
                  // Do nothing
                  assert (block.getNodes.size() == 1 && block.getLastNode.getType.getKind == TypeKind.BOOLEAN)
                } else {
                  logger.debug(s"Potential block ${block.toString}")
                }
              case block: SpecialBlock => assert(assertion = false, "Unreachable")
            }
        })
    })

    null // super.visitMethod(node, p)
  }

  private def isDeltaVariable(identifier: String): Boolean = {
    identifier match {
      case deltaVariablePattern() => true
      case _ => false
    }
  }

  private def extractDeltaVariableFromAssignment(cfgNode: Node): Option[String] = {
    cfgNode match {
      case node: AssignmentNode =>
        // Must be in the form of d = d + e
        if (isDeltaVariable(node.getTarget.toString)) {
          val deltaVariable = node.getTarget.toString
          node.getExpression match {
            case node: NumericalAdditionNode => if (node.getLeftOperand.toString == deltaVariable) Some(deltaVariable) else None
            case _ => None
          }
        }
        else None
      case _ => None
    }
  }

  private def getEnclosingClass(node: MethodTree): ClassTree = TreeUtils.enclosingClass(atypeFactory.getPath(node))

  private def getLineNumber(node: Tree): Int = {
    def getLineNumber(node: Tree, positions: SourcePositions, root: CompilationUnitTree): Long = {
      root.getLineMap.getLineNumber(positions.getStartPosition(root, node))
    }

    val line_long = getLineNumber(node, positions, root)
    assert(line_long <= Integer.MAX_VALUE, "line number overflows")
    line_long.toInt
  }

  private def getFileName: String = root.getSourceFile.getName
}

case class AssignmentToDeltaVariable(cfgNode: Node, deltaVariable: String) {
  assert(cfgNode.isInstanceOf[AssignmentNode])
}
