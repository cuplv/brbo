package bndinfchecker

import com.sun.source.tree._
import com.sun.source.util.SourcePositions
import javax.lang.model.`type`.TypeKind
import org.apache.logging.log4j.LogManager
import org.checkerframework.common.basetype.{BaseAnnotatedTypeFactory, BaseTypeChecker, BaseTypeVisitor}
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.UnderlyingAST.CFGMethod
import org.checkerframework.dataflow.cfg.block.Block.BlockType
import org.checkerframework.dataflow.cfg.block._
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
    logger.debug(s"Visiting method ${node.getName} in file $getFileName")

    val underlyingAST = new CFGMethod(node, getEnclosingClass(node))
    val cfg: ControlFlowGraph = CFGBuilder.build(root, underlyingAST, false, true, this.checker.getProcessingEnvironment)

    CFGUtils.printPDF(cfg)

    val assignmentsToDeltaVariables = cfg.getAllNodes.asScala.foldLeft(List[AssignmentToDeltaVariable]())({
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
    })
    val allBlocks: Set[Block] = cfg.getAllBlocks.asScala.toSet
    val potentialAccumulationContexts: List[(AssignmentToDeltaVariable, Set[Block])] = assignmentsToDeltaVariables.map({
      assignmentToDeltaVariable =>
        logger.debug(s"Finding potential accumulation contexts for $assignmentToDeltaVariable")

        val thisBlock = assignmentToDeltaVariable.cfgNode.getBlock
        val potentialBlocks = allBlocks.filter({
          block => (block != thisBlock) && CFGUtils.existsPath(block, thisBlock) && CFGUtils.existsPath(thisBlock, block)
        }).filter({
          case _: ConditionalBlock => false // Do nothing
          case _: ExceptionBlock => false // Do nothing
          case block: RegularBlock =>
            if (block.getSuccessor.getType == BlockType.CONDITIONAL_BLOCK) {
              // Do nothing
              assert(block.getNodes.size() == 3)
              assert(block.getLastNode.getType.getKind == TypeKind.BOOLEAN)
              false
            } else {
              logger.debug(s"Find a potential block ${block.toString}")
              block.getNodes.asScala.foreach({
                node => logger.trace(s"Node $node is mapped to AST tree ${node.getTree}")
              })
              true
            }
          case _: SpecialBlock => assert(assertion = false, "Unreachable"); false
        })
        (assignmentToDeltaVariable, potentialBlocks)
    })

    potentialAccumulationContexts.foreach({
      case (assignmentToDeltaVariable, potentialBlocks) =>
        if (potentialBlocks.nonEmpty)
          logger.debug(s"Inserting accumulation contexts for $assignmentToDeltaVariable")
        else
          logger.debug(s"No place to insert accumulation contexts for $assignmentToDeltaVariable")
        potentialBlocks.foreach({
          block =>
            logger.debug(s"Inserting at the end of block $block")
            val instrumented = Instrument.instrumentTreeByConvertingToString(node.getBody, assignmentToDeltaVariable.deltaVariable, block, 0, cfg)

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

  case class AssignmentToDeltaVariable(cfgNode: Node, deltaVariable: String) {
    assert(cfgNode.isInstanceOf[AssignmentNode])

    override def toString: String = s"delta variable $deltaVariable in stmt $cfgNode at line ${getLineNumber(cfgNode.getTree)}"
  }

}
