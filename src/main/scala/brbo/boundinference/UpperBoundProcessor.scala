package brbo.boundinference

import brbo.boundinference.FileFormat.C_FORMAT
import brbo.common.Instrument.AtomicStatementInstrumentation
import brbo.common.Instrument.GhostVariable.{Counter, Delta}
import brbo.common.Instrument.InstrumentMode.AT_MOST_ONCE
import brbo.common.icra.Icra
import brbo.common.{CFGUtils, Instrument}
import com.sun.source.tree.Tree
import javax.annotation.processing.SupportedAnnotationTypes
import javax.lang.model.`type`.TypeKind
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.block.Block.BlockType
import org.checkerframework.dataflow.cfg.block._
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node}

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, SortedSet}

/**
 * @param sourceCodeNoResourceUpdates The source code without any updates to resource variables
 * @param targetGhostVariable         The ghost variable that we wish to infer an upper bound
 * @param boundVocabulary             The vocabulary used in the upper bound
 */
@SupportedAnnotationTypes(Array("*"))
class UpperBoundProcessor(sourceCodeNoResourceUpdates: String, targetGhostVariable: String, boundVocabulary: Set[String]) extends BasicProcessor {
  private val logger = LogManager.getLogger(classOf[UpperBoundProcessor])

  private val basicBlockOrdering = Ordering.by { e: Block => e.getUid }

  override def runAnalysis(): Unit = {
    assumeOneClassOneMethod()

    // For each d = d + e, generate a file with d = 0 inserted and another file with c = 0 and c = c + 1 inserted
    val methodBodiesCFormat = {
      val methodBodiesJavaFormat = placeAccumulationContexts()
      getMethods.head match {
        case (methodTree, _) =>
          val className = getEnclosingClass(methodTree).get.getSimpleName.toString
          methodBodiesJavaFormat.map(methodBody => replaceMethodBodyAndGenerateSourceCode(methodTree, className, methodBody, C_FORMAT))
      }
    }

    methodBodiesCFormat.foreach({
      attempt =>
        Icra.run(attempt)
    })
  }

  def placeAccumulationContexts(): List[String] = {
    getMethods.head match {
      case (methodTree, cfg) =>
        val assignmentsToGhostVariables: Map[String, List[Node]] =
          cfg.getAllNodes.asScala.foldLeft(new HashMap[String, List[Node]])({
            (acc, cfgNode) =>
              cfgNode match {
                case cfgNode: AssignmentNode => Instrument.extractGhostVariableFromAssignment(cfgNode, List(Delta, Counter)) match {
                  case Some(ghostVariableUpdate) =>
                    // We avoid using hash sets to store results, because we may have different
                    // assignments that will be determined as a same assignment in a hash set
                    val ghostVariable = ghostVariableUpdate.identifier
                    acc + (ghostVariable -> (cfgNode :: acc.getOrElse(ghostVariable, List[Node]())))
                  case _ => acc
                }
                case _ => acc
              }
          })

        val allBlocks: Set[Block] = cfg.getAllBlocks.asScala.toSet
        val potentialAccumulationContexts: Map[String, SortedSet[Block]] = assignmentsToGhostVariables.map({
          case (assignmentToDeltaVariable, assignmentNodes) =>
            logger.debug(s"Finding potential accumulation contexts for `$assignmentToDeltaVariable`")

            val assignmentBlocks = assignmentNodes.map(node => node.getBlock).toSet
            val potentialBlocks: Set[Block] = allBlocks.filter({
              block =>
                assignmentBlocks.exists(assignmentBlock =>
                  (block != assignmentBlock) && CFGUtils.existsPath(block, assignmentBlock) && CFGUtils.existsPath(assignmentBlock, block)
                )
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
                  logger.debug(s"Find a potential block `${block.toString}`")
                  block.getNodes.asScala.foreach({
                    node => logger.trace(s"Node `$node` is mapped to AST tree `${node.getTree}`")
                  })
                  true
                }
              case _: SpecialBlock => throw new RuntimeException("Unreachable")
            })
            (assignmentToDeltaVariable, SortedSet[Block]()(basicBlockOrdering) ++ potentialBlocks)
        })

        // The order of instrumented code is useful for matching `d = 0` with `c = c + 1`
        potentialAccumulationContexts.foldLeft(List[String]())({
          case (acc, (ghostVariable, potentialBlocks)) =>
            if (potentialBlocks.nonEmpty) {
              logger.debug(s"Inserting accumulation contexts for ghost variable `$ghostVariable`")
              val instrumentedCode: SortedSet[String] = potentialBlocks.map({
                potentialBlock: Block =>
                  logger.debug(s"Instrumenting block `$potentialBlock`")
                  val result = Instrument.substituteAtomicStatements(
                    methodTree.getBody,
                    AtomicStatementInstrumentation(
                      {
                        node: Node => potentialBlock.getNodes.contains(node)
                      },
                      {
                        tree: Tree => s"${tree.toString}; $ghostVariable = 0;"
                      }
                    ),
                    indent,
                    cfg,
                    getLineNumber,
                    AT_MOST_ONCE
                  )
                  result.result
              })
              // TODO: Insert `assert(1)` so that ICRA can infer invariants at these locations
              acc ++ instrumentedCode
            }
            else {
              logger.debug(s"No place to insert accumulation contexts for ghost variable `$ghostVariable`")
              acc
            }
        })
    }
  }
}
