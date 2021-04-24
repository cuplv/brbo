package brbo.verification.decomposition

import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta, Resource}
import brbo.common.{CFGUtils, GhostVariableUtils, TargetMethod, TreeUtils}
import brbo.verification.AmortizationMode.AmortizationMode
import brbo.verification.BasicProcessor
import brbo.verification.dependency.reachdef.ReachingValue
import brbo.verification.dependency.{ControlDependency, DataDependency}
import com.sun.source.tree.{ExpressionStatementTree, ExpressionTree, StatementTree}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.block.Block.BlockType
import org.checkerframework.dataflow.cfg.block.{Block, ConditionalBlock}
import org.checkerframework.dataflow.cfg.node._

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

object DecompositionUtils {

  private val logger = LogManager.getLogger("brbo.verification.decomposition.DecompositionUtils")

  def traceOrError(message: String, debug: Boolean): Unit = {
    if (debug) logger.error(message)
    else logger.trace(message)
  }

  /**
   *
   * @param statement    The statement from which we generate an initial subprogram
   * @param targetMethod The method that encloses the above statement
   * @return The initial program and its entry node in the CFG
   */
  def initializeSubprogramFromStatement(statement: StatementTree, targetMethod: TargetMethod): Option[(StatementTree, Node)] = {
    statement match {
      case expressionStatementTree: ExpressionStatementTree =>
        GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource) match {
          case Some(updateTree) =>
            val updateNode = CFGUtils.getNodesCorrespondingToExpressionStatementTree(statement, targetMethod.cfg)

            updateTree.increment match {
              /*case literalTree: LiteralTree =>
                assert(literalTree.getKind == Kind.INT_LITERAL)
                // The initial subprogram is the minimal enclosing loop when `R` is updated by a constant
                val subprogram: StatementTree = {
                  TreeUtils.getMinimalEnclosingLoop(targetMethod.getPath(statement)) match {
                    case Some(enclosingLoop) => enclosingLoop
                    case None =>
                      logger.trace(s"Resource update `$statement` does not have an enclosing loop")
                      statement
                  }
                }.asInstanceOf[StatementTree]
                val entryNode: Node = CFGUtils.entryOfMinimalEnclosingLoop(updateNode, targetMethod) match {
                  case Some(entryNode) => entryNode
                  case None => updateNode
                }
                logger.trace(s"Resource update `$statement`'s initial subprogram is `$subprogram`. Entry node is `$entryNode`")
                Some(subprogram, entryNode)*/
              case _ => Some(statement, updateNode)
            }
          case None => None
        }
      case _ => None
    }
  }

  def computeModifiedSet(targetMethod: TargetMethod): Set[String] = {
    targetMethod.cfg.getAllNodes.asScala
      .foldLeft(new HashSet[String])({
        (acc, node) =>
          node match {
            case assignmentNode: AssignmentNode => acc + assignmentNode.getTarget.toString
            case _ => acc
          }
      })
      .filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource))
  }

  /**
   *
   * @param targetMethod The method to compute a taint set
   * @return Compute the variables that any `r+=e` in the input method data depends on
   */
  @deprecated
  def computeTaintSet(targetMethod: TargetMethod, debug: Boolean): Set[String] = {
    val dataDependency = DataDependency.computeDataDependency(targetMethod)
    val controlDependency = {
      val controlDependency = ControlDependency.computeControlDependency(targetMethod)
      ControlDependency.reverseControlDependency(controlDependency)
    }

    val taintSet = TreeUtils.collectCommands(targetMethod.methodTree.getBody).flatMap({
      statement =>
        initializeSubprogramFromStatement(statement, targetMethod) match {
          case Some((initialSubprogram, entryNode)) =>
            traceOrError(s"Compute taint set for `$initialSubprogram`", debug)
            val conditionTrees = TreeUtils.collectConditionTreesWithoutBrackets(initialSubprogram)

            val correspondingNode = CFGUtils.getNodesCorrespondingToExpressionStatementTree(statement, targetMethod.cfg)
            traceOrError(s"Expression Tree `$statement` corresponds to node `$correspondingNode`", debug)

            // Treat `r+=e` when `e` is constant differently from `r+=e` when `e` is constant
            val dataDependentVariables: Set[String] = GhostVariableUtils.extractUpdate(correspondingNode, Resource) match {
              case Some(update) =>
                update.increment match {
                  case updateNode: ValueLiteralNode =>
                    traceOrError(s"The update in `$statement` is constant", debug)
                    updateNode match {
                      case _: IntegerLiteralNode =>
                        computeTransitiveControlDependency(correspondingNode, controlDependency, new HashSet[Node], debug).flatMap({
                          conditionNode =>
                            val conditionTree = conditionNode.getTree.asInstanceOf[ExpressionTree]
                            traceOrError(s"The condition node is `$conditionNode`", debug)
                            logger.trace(s"The condition tree is `$conditionTree` (hash code: `${conditionTree.hashCode()}`)")
                            conditionTrees.foreach(t => logger.trace(s"Condition tree `$t` (hash code: `${t.hashCode()}`)"))
                            if (conditionTrees.contains(conditionTree)) {
                              CFGUtils.getUsedVariables(conditionNode)
                            }
                            else new HashSet[String]
                        })
                      case _ => throw new Exception(s"Unsupported literal node `$updateNode`")
                    }
                  case _ =>
                    traceOrError(s"The update in statement `$statement` is not constant", debug)
                    CFGUtils.getUsedVariables(correspondingNode.asInstanceOf[AssignmentNode].getExpression)
                }
              case None => throw new Exception("Unexpected")
            }
            traceOrError(s"Data dependent variables for AST `$statement`'s initial program are `$dataDependentVariables`", debug)

            // Compute transitive data dependency for the above variables, starting from the entry node of initial subprogram
            computeTransitiveDataDependencyInputsOnly(dataDependentVariables, entryNode, dataDependency, debug)
          case None => new HashSet[String]
        }
    })
    taintSet.filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource)).toSet
  }

  @deprecated
  private def computeTransitiveDataDependencyInputsOnly(seedVariables: Set[String],
                                                        initialLocation: Node,
                                                        dataDependency: Map[Node, Set[ReachingValue]],
                                                        debug: Boolean): Set[String] = {
    dataDependency.get(initialLocation) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => seedVariables.contains(definition.variable))
        newDefinitions.flatMap({
          definition =>
            definition.node match {
              case Some(n) =>
                computeTransitiveDataDependencyInputsOnlyHelper(n, dataDependency, HashSet[Node](initialLocation), debug)
              case None => HashSet[String](definition.variable) // This definition comes from an input variable
            }
        })
      case None => new HashSet[String]
    }
  }

  @deprecated
  private def computeTransitiveDataDependencyInputsOnlyHelper(node: Node,
                                                              dataDependency: Map[Node, Set[ReachingValue]],
                                                              visited: Set[Node],
                                                              debug: Boolean): Set[String] = {
    if (visited.contains(node) || node == null) return new HashSet[String]
    traceOrError(s"Compute data dependency for node `$node`", debug)

    val usedVariables: Set[String] = node match {
      case assignmentNode: AssignmentNode => CFGUtils.getUsedVariables(assignmentNode.getExpression)
      case _: VariableDeclarationNode => new HashSet[String]
      case _ => throw new Exception(s"Expecting assignment or variable declaration node `$node` (Type: `${node.getClass}`)")
    }
    traceOrError(s"Used variables: $usedVariables", debug)

    dataDependency.get(node) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        traceOrError(s"Reaching definitions: $newDefinitions", debug)
        newDefinitions.flatMap({
          definition =>
            definition.node match {
              case Some(n) => computeTransitiveDataDependencyInputsOnlyHelper(n, dataDependency, visited + node, debug)
              case None => HashSet[String](definition.variable) // This definition comes from an input variable
            }
        })
      case None => new HashSet[String]
    }
  }

  @deprecated
  private def computeTransitiveControlDependency(node: Node,
                                                 controlDependency: Map[Block, Set[Block]],
                                                 visited: Set[Node],
                                                 debug: Boolean): Set[Node] = {
    assert(node != null)
    if (visited.contains(node)) return new HashSet[Node]
    traceOrError(s"Compute transitive control dependency for node `$node`", debug)

    controlDependency.get(node.getBlock) match {
      case Some(blocks) =>
        blocks.flatMap({
          block =>
            getCondition(node, block, debug) match {
              case Some(condition2) => computeTransitiveControlDependency(condition2, controlDependency, visited + node, debug) + condition2
              case None => new HashSet[Node]
            }
        })
      case None => new HashSet[Node]
    }
  }

  /**
   *
   * @param node  A node
   * @param block A block that controls the enclosing block of the input node
   * @param debug Print debug information
   * @return The condition node that the block is control-dependent on
   */
  private def getCondition(node: Node, block: Block, debug: Boolean): Option[Node] = {
    def blocksToString(blocks: Iterable[Block]): String = {
      val body = blocks.map(block => s"${block.getUid}: $block").mkString("\n")
      s"====\n$body\n===="
    }

    assert(block.isInstanceOf[ConditionalBlock], s"Block ${node.getBlock.getUid} control depends on block ${block.getUid}")
    val predecessors = block.getPredecessors.asScala
    assert(predecessors.size == 1)

    val predecessor = predecessors.head
    val condition = predecessor.getLastNode
    if (condition == null) {
      traceOrError(s"${node.getBlock}'s predecessor block does not have a condition: `$predecessor`", debug)
      if (predecessor.getType == BlockType.EXCEPTION_BLOCK) {
        traceOrError(s"${node.getBlock}'s predecessor block is an exception block: `$predecessor`", debug)

        val predecessors2 = predecessor.getPredecessors.asScala
        assert(predecessors2.size == 1, s"\n${blocksToString(predecessors2)}")
        traceOrError(s"The last node (predecessor^2) in the exception block is: `${predecessors2.head.getLastNode}`", debug)

        // val predecessors3 = predecessors2.head.getPredecessors.asScala
        // assert(predecessors3.size == 1, s"\n${blocksToString(predecessors2)}\n${blocksToString(predecessors3)}")
        // traceOrError(s"The last node (predecessor^3) is: `${predecessors3.head.getLastNode}`", debug)

        // val predecessors4 = predecessors3.head.getPredecessors.asScala
        // assert(predecessors4.size == 1, s"\n${blocksToString(predecessors2)}\n${blocksToString(predecessors3)}\n${blocksToString(predecessors4)}")
        // traceOrError(s"The last node (predecessor^4) is: `${predecessors4.head.getLastNode}`", debug)

        Some(predecessors2.head.getLastNode)
      }
      else None
    }
    else Some(condition)
  }

  def computeTaintSetControlAndData(targetMethod: TargetMethod, debug: Boolean): Set[String] = {
    val dataDependency = DataDependency.computeDataDependency(targetMethod)
    val controlDependency = {
      val controlDependency = ControlDependency.computeControlDependency(targetMethod)
      ControlDependency.reverseControlDependency(controlDependency)
    }
    targetMethod.cfg.getAllNodes.asScala
      .filter({ node => GhostVariableUtils.extractUpdate(node, Resource).isDefined })
      .flatMap({ node =>
        traceOrError(s"Compute taint set for $node", debug)
        computeTaintSetControlAndDataHelper(node, isExpression = false, dataDependency, controlDependency, new HashSet[Node], debug)
      })
      .toSet
  }

  private def computeTaintSetControlAndDataHelper(inputNode: Node,
                                                  isExpression: Boolean,
                                                  dataDependency: Map[Node, Set[ReachingValue]],
                                                  controlDependency: Map[Block, Set[Block]],
                                                  visited: Set[Node],
                                                  debug: Boolean): Set[String] = {
    if (visited.contains(inputNode)) return new HashSet[String]

    val usedVariables: Set[String] = {
      if (isExpression) CFGUtils.getUsedVariables(inputNode)
      else {
        inputNode match {
          case assignmentNode: AssignmentNode => CFGUtils.getUsedVariables(assignmentNode.getExpression)
          case _: VariableDeclarationNode => new HashSet[String]
          case _ => throw new Exception(s"Expect assignment or variable declaration node `$inputNode` (Type: `${inputNode.getClass}`)")
        }
      }
    }
    traceOrError(s"Used variables: $usedVariables", debug)

    val set1: Set[String] = dataDependency.get(inputNode) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        traceOrError(s"Reaching definitions (that are used): $newDefinitions", debug)
        newDefinitions.flatMap({
          definition =>
            definition.node match {
              case Some(node) => computeTaintSetControlAndDataHelper(node, isExpression = false, dataDependency, controlDependency, visited + inputNode, debug)
              case None => HashSet[String](definition.variable) // This definition comes from an input variable
            }
        })
      case None => new HashSet[String]
    }

    val set2: Set[String] = controlDependency.get(inputNode.getBlock) match {
      case Some(blocks) =>
        blocks.flatMap({
          block =>
            traceOrError(s"Compute taint set for conditional node $inputNode", debug)
            getCondition(inputNode, block, debug) match {
              case Some(condition) => computeTaintSetControlAndDataHelper(condition, isExpression = true, dataDependency, controlDependency, visited + inputNode, debug)
              case None => new HashSet[String]
            }
        })
      case None => new HashSet[String]
    }

    set1 ++ set2
  }

  case class DeltaCounterPair(delta: String, counter: String) {
    GhostVariableUtils.isGhostVariable(delta, Delta)
    GhostVariableUtils.isGhostVariable(counter, Counter)
  }

  /**
   *
   * @param newSourceFileContents The source code after decomposing the input method
   * @param deltaCounterPairs     The paris of delta variables and counters in the new source code
   * @param amortizationMode      The mode that was used to construct this decomposition
   * @param inputMethod           The input method that was decomposed
   */
  case class DecompositionResult(newSourceFileContents: String,
                                 deltaCounterPairs: Set[DeltaCounterPair],
                                 amortizationMode: AmortizationMode,
                                 inputMethod: TargetMethod) {
    logger.info(s"Decomposition result (Mode: `$amortizationMode`):\n$newSourceFileContents")
    val outputMethod: TargetMethod = BasicProcessor.getTargetMethod(inputMethod.fullQualifiedClassName, newSourceFileContents)
    assert(outputMethod.inputVariables == inputMethod.inputVariables)
  }

}
