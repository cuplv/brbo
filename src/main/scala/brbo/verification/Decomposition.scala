package brbo.verification

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.{CFGUtils, GhostVariableUtils, TargetMethod, TreeUtils}
import brbo.verification.dependency.reachdef.ReachingValue
import brbo.verification.dependency.{ControlDependency, DataDependency}
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.block.{Block, ConditionalBlock}
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node}

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

class Decomposition(inputMethod: TargetMethod) {
  private val logger = LogManager.getLogger(classOf[Decomposition])

  private val commands = TreeUtils.collectCommands(inputMethod.methodTree.getBody)

  def decompose(): String = {
    var subprograms = initializeSubprograms()
    var continue = true
    while (continue) {
      val interferedSubprograms = findInterference(subprograms)
      if (interferedSubprograms.isEmpty) {
        continue = false
      }
      else {
        val pair = interferedSubprograms.head
        logger.error(s"Subprogram ${pair._1} interferes with ${pair._2}")
        if (pair._1 == pair._2) {
          subprograms = enlargeNoInterference(pair._1, subprograms)
        }
        else {
          val newSubprogram = merge(pair._1, pair._2)
          subprograms = mergeIfOverlap(subprograms.programs - pair._1 - pair._2 + newSubprogram)
        }
      }
    }
    insertResets(subprograms)
  }

  def initializeSubprograms(): Subprograms = {
    val initialSubprograms = commands.foldLeft(new HashSet[Subprogram])({
      (acc, statement) =>
        GhostVariableUtils.extractGhostVariableUpdate(statement, Resource) match {
          case Some(updateNode) =>
            updateNode.update match {
              case _: LiteralTree =>
                // The initial subprogram is the minimal enclosing loop
                val subprogram = {
                  inputMethod.getMinimalEnclosingLoop(statement) match {
                    case Some(enclosingLoop) => enclosingLoop
                    case None => statement
                  }
                }
                logger.error(s"Resource update `$statement`'s initial subprogram is `$subprogram`")
                acc + Subprogram(List(subprogram))
              case _ => acc + Subprogram(List(statement))
            }
          case None => acc
        }
    })
    eliminateEnvironmentInterference(mergeIfOverlap(initialSubprograms))
  }

  def insertResets(subprograms: Subprograms): String = {
    ???
  }

  /**
   *
   * @param subprogram The subprogram to be enlarged
   * @return A new subprogram such that
   *         - It encloses the input subprogram
   *         - It does not overlap with other subprograms
   *         - No interference: It does not interfere with itself
   *         - No interference: The environment does not interfere with it
   */
  def enlargeNoInterference(subprogram: Subprogram, subprograms: Subprograms): Subprograms = {
    assert(subprograms.programs.contains(subprogram))
    ???
  }

  def merge(subprogram1: Subprogram, subprogram2: Subprogram): Subprogram = {
    val minimalCommonTree = {
      if (subprogram2.astNodes.size == 1)
        subprogram2.astNodes.head
      else {
        inputMethod.getPath(subprogram2.astNodes.head).getParentPath.getLeaf match {
          case blockTree: BlockTree => blockTree
          case _ => throw new Exception(s"Unexpected")
        }
      }
    }

    var p = inputMethod.getPath(subprogram1.astNodes.head)
    while (p != null) {
      val leaf: Tree = p.getLeaf
      assert(leaf != null)
      if (leaf == minimalCommonTree)
        return Subprogram(List(leaf))
      p = p.getParentPath
    }

    throw new Exception(s"Unable to merge $subprogram1 and $subprogram2\nMinimal common tree of Subprogram2 is $minimalCommonTree")
  }

  def eliminateEnvironmentInterference(subprograms: Subprograms): Subprograms = {
    var newSubprograms = subprograms
    var continue = true
    while (continue) {
      newSubprograms.programs.find(subprogram => interferedByEnvironment(subprogram, newSubprograms)) match {
        case Some(interferedSubprogram) =>
          newSubprograms = enlarge(interferedSubprogram, newSubprograms)
        case None => continue = false
      }
    }
    newSubprograms
  }

  /**
   *
   * @param subprogram The subprogram to be enlarged
   * @return A new subprogram such that
   *         - It encloses the input subprogram
   *         - It does not overlap with other subprograms
   */
  def enlarge(subprogram: Subprogram, subprograms: Subprograms): Subprograms = {
    assert(subprograms.programs.contains(subprogram))

    val headAstNode = subprogram.astNodes.head
    val newAstNodes: List[Tree] = inputMethod.getPath(headAstNode).getParentPath.getLeaf match {
      case null =>
        logger.error(s"Enlarging but there is no enclosing tree for $headAstNode")
        List(headAstNode)
      case immediateEnclosingAstNode =>
        immediateEnclosingAstNode match {
          case blockTree: BlockTree =>
            val statements = blockTree.getStatements
            statements.indexOf(headAstNode) match {
              case -1 => throw new Exception(s"Unexpected")
              case index =>
                if (index == 0) {
                  statements.indexOf(subprogram.astNodes.last) match {
                    case -1 => throw new Exception(s"Unexpected")
                    case index2 =>
                      if (index2 == statements.size() - 1) {
                        List(inputMethod.getPath(headAstNode).getParentPath.getLeaf)
                      }
                      else {
                        subprogram.astNodes :+ blockTree.getStatements.get(index + 1)
                      }
                  }
                }
                else blockTree.getStatements.get(index - 1) :: subprogram.astNodes
            }
          case tree@(_: IfTree | _: ForLoopTree | _: WhileLoopTree) => List(tree)
          case _ => throw new Exception(s"Unsupported immediate enclosing AST node $immediateEnclosingAstNode")
        }
    }

    mergeIfOverlap(subprograms.programs - subprogram + Subprogram(newAstNodes))
  }

  def mergeIfOverlap(subprograms: Set[Subprogram]): Subprograms = {
    var newSubprograms = subprograms
    var continue = true
    while (continue) {
      newSubprograms.zip(newSubprograms).find(pair => overlap(pair._1, pair._2)) match {
        case Some(pair) =>
          val newSubprogram = merge(pair._1, pair._2)
          newSubprograms = newSubprograms - pair._1 - pair._2 + newSubprogram
        case None => continue = false
      }
    }
    Subprograms(newSubprograms)
  }

  def overlap(subprogram1: Subprogram, subprogram2: Subprogram): Boolean = {
    subprogram1.innerTrees.intersect(subprogram2.innerTrees).nonEmpty
  }

  def environmentModifiedSet(subprograms: Subprograms): Set[String] = {
    ???
  }

  def interferedByEnvironment(subprogram: Subprogram, subprograms: Subprograms): Boolean = {
    assert(subprograms.programs.contains(subprogram))

    val modifiedSet = environmentModifiedSet(subprograms)
    val taintSet = Decomposition.computeTaintSet(subprogram.targetMethod)

    logger.error(s"Environment modified set $modifiedSet overlaps with taintset $taintSet of subprogram $subprogram")
    modifiedSet.intersect(taintSet).nonEmpty
  }

  def findInterference(subprograms: Subprograms): Option[(Subprogram, Subprogram)] = {
    subprograms.programs.zip(subprograms.programs).find({
      case (subprogram1, subprogram2) =>
        if (interfere(subprogram1, subprogram2)) true
        else false
    })
  }

  /**
   *
   * @param subprogram1
   * @param subprogram2
   * @return If subprogram1 may interfere with subprogram2's resource usage
   */
  def interfere(subprogram1: Subprogram, subprogram2: Subprogram): Boolean = {
    // TODO: Check if there exists a path from subprogram1's exit to subprogram2's entry
    val modifiedSet1 = Decomposition.computeModifiedSet(subprogram1.targetMethod)
    val taintSet1 = Decomposition.computeTaintSet(subprogram1.targetMethod)
    val modifiedSet2 = Decomposition.computeModifiedSet(subprogram2.targetMethod)
    val taintSet2 = Decomposition.computeTaintSet(subprogram2.targetMethod)
    modifiedSet1.intersect(taintSet2).nonEmpty || modifiedSet2.intersect(taintSet1).nonEmpty
  }

  case class Subprogram(astNodes: List[Tree]) {
    assert(astNodes.nonEmpty)

    val targetMethod: TargetMethod = BasicProcessor.getTargetMethod(inputMethod.className, toJavaProgram)

    val innerTrees: Set[StatementTree] = astNodes.foldLeft(new HashSet[StatementTree])({
      (acc, astNode) => acc ++ TreeUtils.collectTrees(astNode.asInstanceOf[StatementTree])
    })

    def toJavaProgram: String = {
      ???
    }
  }

  case class Subprograms(programs: Set[Subprogram]) {
    // Any two subprograms should not overlap with each other
    programs.zip(programs).foreach({
      case (program1, program2) =>
        if (program1 != program2) assert(!overlap(program1, program2), s"Overlapping subprograms:\n$program1\n$program2")
    })
  }

}

object Decomposition {
  private val logger = LogManager.getLogger("brbo.boundinference.dependency.Decomposition")

  def computeModifiedSet(targetMethod: TargetMethod): Set[String] = {
    targetMethod.cfg.getAllNodes.asScala.foldLeft(new HashSet[String])({
      (acc, node) =>
        node match {
          case assignmentNode: AssignmentNode => acc + assignmentNode.getTarget.toString
          case _ => acc
        }
    })
  }

  def computeTaintSet(targetMethod: TargetMethod): Set[String] = {
    val dataDependency = DataDependency.computeDataDependency(targetMethod)
    val controlDependency = ControlDependency.computeControlDependency(targetMethod)

    // Treat `r+=e` when `e` is constant differently from `r+=e` when `e` is constant
    ???
  }

  @deprecated
  def computeTaintSetControlAndData(targetMethod: TargetMethod): Set[String] = {
    val dataDependency = DataDependency.computeDataDependency(targetMethod)
    val controlDependency = {
      val controlDependency = ControlDependency.computeControlDependency(targetMethod)
      ControlDependency.reverseControlDependency(controlDependency)
    }
    targetMethod.cfg.getAllNodes.asScala
      .filter({ node => GhostVariableUtils.extractGhostVariableUpdate(node, Resource).isDefined })
      .flatMap({ node =>
        logger.debug(s"Compute taint set for $node")
        computeTaintSetDataHelper(node, dataDependency, controlDependency, new HashSet[Node])
      })
      .toSet
  }

  private def computeTaintSetDataHelper(node: Node,
                                        dataDependency: Map[Node, Set[ReachingValue]],
                                        controlDependency: Map[Block, Set[Block]],
                                        visited: Set[Node]): Set[String] = {
    if (visited.contains(node)) return new HashSet[String]

    val assignmentExpression = node match {
      case assignmentNode: AssignmentNode => assignmentNode.getExpression
      case _ => throw new Exception(s"Compute taint set - Expecting assignment node $node (Type: ${node.getClass})")
    }

    val set1: Set[String] = dataDependency.get(node) match {
      case Some(definitions) =>
        val usedVariables = CFGUtils.getUsedVariables(assignmentExpression)
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        usedVariables ++ computeTaintSetHelper(newDefinitions, dataDependency, controlDependency, visited + node)
      case None => new HashSet[String]
    }

    val set2: Set[String] = controlDependency.get(node.getBlock) match {
      case Some(blocks) =>
        blocks.flatMap({
          block =>
            assert(block.isInstanceOf[ConditionalBlock], s"Block ${node.getBlock.getUid} control depends on block ${block.getUid}")
            val predecessors = block.getPredecessors.asScala
            assert(predecessors.size == 1)

            val condition = predecessors.head.getLastNode
            logger.debug(s"Compute taint set for conditional node $node")
            computeTaintSetControlHelper(condition, dataDependency, controlDependency, visited + node)
        })
      case None => new HashSet[String]
    }

    set1 ++ set2
  }

  private def computeTaintSetControlHelper(node: Node,
                                           dataDependency: Map[Node, Set[ReachingValue]],
                                           controlDependency: Map[Block, Set[Block]],
                                           visited: Set[Node]): Set[String] = {
    if (visited.contains(node)) return new HashSet[String]

    val usedVariables = CFGUtils.getUsedVariables(node)

    val set1 = dataDependency.get(node) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        usedVariables ++ computeTaintSetHelper(newDefinitions, dataDependency, controlDependency, visited + node)
      case None => new HashSet[String]
    }

    val set2 = controlDependency.get(node.getBlock) match {
      case Some(blocks) =>
        blocks.flatMap({
          block =>
            assert(block.isInstanceOf[ConditionalBlock], s"Block ${node.getBlock.getUid} control depends on block ${block.getUid}")
            val predecessors = block.getPredecessors.asScala
            assert(predecessors.size == 1)

            val condition = predecessors.head.getLastNode
            computeTaintSetControlHelper(condition, dataDependency, controlDependency, visited + node)
        })
      case None => new HashSet[String]
    }

    set1 ++ set2
  }

  private def computeTaintSetHelper(definitions: Iterable[ReachingValue],
                                    dataDependency: Map[Node, Set[ReachingValue]],
                                    controlDependency: Map[Block, Set[Block]],
                                    visited: Set[Node]): Set[String] = {
    definitions.flatMap({
      definition =>
        definition.node match {
          case Some(n) =>
            logger.debug(s"Compute taint set - Recursive call to $n")
            computeTaintSetDataHelper(n, dataDependency, controlDependency, visited)
          case None => HashSet[String](definition.variable) // This definition comes from an input variable
        }
    }).toSet
  }
}