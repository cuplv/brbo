package brbo.verification

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.InstrumentUtils.FileFormat.JAVA_FORMAT
import brbo.common.TypeUtils.BrboType
import brbo.common._
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
    var subprograms = eliminateEnvironmentInterference(mergeIfOverlap(initializeSubprograms()))
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

  def initializeSubprograms(): Set[Subprogram] = {
    commands.foldLeft(new HashSet[Subprogram])({
      (acc, statement) =>
        statement match {
          case expressionStatementTree: ExpressionStatementTree =>
            GhostVariableUtils.extractGhostVariableUpdate(expressionStatementTree.getExpression, Resource) match {
              case Some(updateNode) =>
                updateNode.update match {
                  case _: LiteralTree =>
                    // The initial subprogram is the minimal enclosing loop when `R` is updated by a constant
                    val subprogram = {
                      inputMethod.getMinimalEnclosingLoop(statement) match {
                        case Some(enclosingLoop) => enclosingLoop
                        case None =>
                          logger.trace(s"Resource update `$statement` does not have an enclosing loop")
                          statement
                      }
                    }
                    logger.trace(s"Resource update `$statement`'s initial subprogram is `$subprogram`")
                    acc + Subprogram(List(subprogram.asInstanceOf[StatementTree]))
                  case _ => acc + Subprogram(List(statement))
                }
              case None => acc
            }
          case _ => acc
        }
    })
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
    (subprogram1.minimalEnclosingBlock, subprogram2.minimalEnclosingBlock) match {
      case (Some(minimalEnclosingBlock1), Some(minimalEnclosingBlock2))
        if minimalEnclosingBlock1 == minimalEnclosingBlock2 =>
        val head = {
          val head1 = minimalEnclosingBlock1.getStatements.indexOf(subprogram1.astNodes.head)
          val head2 = minimalEnclosingBlock1.getStatements.indexOf(subprogram2.astNodes.head)
          assert(head1 != -1 && head2 != -1)
          if (head1 <= head2) head1 else head2
        }
        val last = {
          val last1 = minimalEnclosingBlock1.getStatements.indexOf(subprogram1.astNodes.last)
          val last2 = minimalEnclosingBlock1.getStatements.indexOf(subprogram2.astNodes.last)
          assert(last1 != -1 && last2 != -1)
          if (last1 >= last2) last1 else last2
        }
        return Subprogram(minimalEnclosingBlock1.getStatements.asScala.slice(head, last + 1).toList)
      case _ =>
        var p = inputMethod.getPath(subprogram1.astNodes.head)
        while (p != null) {
          val leaf: Tree = p.getLeaf
          assert(leaf != null)
          val allTrees = TreeUtils.collectTrees(leaf.asInstanceOf[StatementTree])
          if (subprogram2.innerTrees.subsetOf(allTrees)) {
            leaf match {
              case blockTree: BlockTree =>
                val statements = blockTree.getStatements.asScala.toList
                val statements2 = statements.filter({
                  statement =>
                    val trees = TreeUtils.collectTrees(statement)
                    trees.intersect(subprogram1.innerTrees).nonEmpty || trees.intersect(subprogram2.innerTrees).nonEmpty
                })
                val startIndex = statements.indexOf(statements2.head)
                val endIndex = statements.indexOf(statements2.last)
                assert(startIndex != -1 && endIndex != -1)
                return Subprogram(statements.slice(startIndex, endIndex + 1))
              case _ => return Subprogram(List(leaf.asInstanceOf[StatementTree]))
            }
          }
          p = p.getParentPath
        }
    }

    throw new Exception(s"Unable to merge $subprogram1 and $subprogram2")
  }

  def eliminateEnvironmentInterference(subprograms: Subprograms): Subprograms = {
    var newSubprograms: Subprograms = subprograms
    var continue = true
    while (continue) {
      newSubprograms.programs.find(subprogram => interferedByEnvironment(subprogram, newSubprograms)) match {
        case Some(interferedSubprogram) =>
          val newSubprogram: Subprogram = enlarge(interferedSubprogram)
          newSubprograms = mergeIfOverlap(newSubprograms.programs - interferedSubprogram + newSubprogram)
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
  def enlarge(subprogram: Subprogram): Subprogram = {
    val headAstNode = subprogram.astNodes.head
    val newAstNodes: List[Tree] = subprogram.minimalEnclosingBlock match {
      case Some(minimalEnclosingBlock) =>
        val statements = minimalEnclosingBlock.getStatements
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
                    subprogram.astNodes :+ statements.get(index + 1)
                  }
              }
            }
            else statements.get(index - 1) :: subprogram.astNodes
        }
      case None =>
        inputMethod.getPath(subprogram.minimalEnclosingTree).getParentPath.getLeaf match {
          case null =>
            logger.error(s"There is no minimal enclosing tree for minimal enclosing tree ${subprogram.minimalEnclosingTree}")
            List(headAstNode)
          case minimalEnclosingTree2 =>
            minimalEnclosingTree2 match {
              case tree@(_: IfTree | _: ForLoopTree | _: WhileLoopTree) => List(tree)
              case _ => throw new Exception(s"Unsupported minimal enclosing AST node $minimalEnclosingTree2")
            }
        }
    }

    Subprogram(newAstNodes.map(tree => tree.asInstanceOf[StatementTree]))
  }

  def mergeIfOverlap(subprograms: Set[Subprogram]): Subprograms = {
    var newSubprograms: Set[Subprogram] = subprograms
    var continue = true
    while (continue) {
      MathUtils.crossJoin2(newSubprograms, newSubprograms)
        .filter(pair => pair._1 != pair._2)
        .find({ pair => overlap(pair._1, pair._2) })
      match {
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
    // subprograms.head, subprograms.tail.head
    MathUtils.crossJoin(List(subprograms.programs, subprograms.programs)).find({
      subprograms2 =>
        if (interfere(subprograms2.head, subprograms2.tail.head)) true
        else false
    }) match {
      case Some(subprograms3) => Some(subprograms3.head, subprograms3.tail.head)
      case None => None
    }
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

  case class Subprogram(astNodes: List[StatementTree]) {
    assert(astNodes.nonEmpty)

    override def toString: String = {
      val nodes = astNodes.map(t => t.toString).mkString("\n")
      s"Subprogram(\n$nodes\n)"
    }

    val innerTrees: Set[StatementTree] = astNodes.foldLeft(new HashSet[StatementTree])({
      (acc, astNode) => acc ++ TreeUtils.collectTrees(astNode)
    })

    val minimalEnclosingTree: StatementTree = {
      if (astNodes.size == 1)
        astNodes.head
      else {
        inputMethod.getPath(astNodes.head).getParentPath.getLeaf match {
          case blockTree: BlockTree => blockTree
          case null => throw new Exception(s"No minimal enclosing tree for $this")
        }
      }
    }

    val minimalEnclosingBlock: Option[BlockTree] = {
      org.checkerframework.javacutil.TreeUtils.enclosingOfKind(inputMethod.getPath(astNodes.head), Tree.Kind.BLOCK) match {
        case null => throw new Exception("Unexpected")
        case blockTree: BlockTree =>
          if (blockTree.getStatements.contains(astNodes.head)) Some(blockTree)
          else None
      }
    }
    assert(minimalEnclosingBlock.isEmpty || astNodes.size == 1 || minimalEnclosingTree == minimalEnclosingBlock.get)
    assert(minimalEnclosingBlock.nonEmpty || minimalEnclosingTree == astNodes.head)

    private val declaredLocalVariables = TreeUtils.collectCommands(astNodes).foldLeft(new HashSet[String])({
      (acc, tree) =>
        tree match {
          case variableTree: VariableTree => acc + variableTree.getName.toString
          case _ => acc
        }
    }).toList.sortWith(_ < _)

    val javaProgramRepresentation: String = {
      // Assume all variables have distinct names
      val parameters =
        (inputMethod.inputVariables ++ inputMethod.localVariables)
          .filterKeys(key => !declaredLocalVariables.contains(key))
          .map({ case (identifier, typ) => BrboType.variableDeclaration(identifier, typ) })
          .mkString(", ")
      val methodBody = astNodes.map(tree => tree.toString).mkString("\n")
      InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(
        inputMethod,
        Some(parameters),
        s"{\n$methodBody\n}",
        JAVA_FORMAT,
        2
      )
    }

    val targetMethod: TargetMethod = BasicProcessor.getTargetMethod(inputMethod.className, javaProgramRepresentation)
  }

  case class Subprograms(programs: Set[Subprogram]) {
    // Any two subprograms should not overlap with each other
    MathUtils.crossJoin(List(programs, programs)).foreach({
      programs2 =>
        val program1 = programs2.head
        val program2 = programs2.tail.head
        if (program1 != program2) assert(!overlap(program1, program2), s"Overlapping subprograms:\n$program1\n$program2")
    })

    override def toString: String = {
      val subprograms = programs.map(x => x.toString).toList.mkString("\n")
      s"Subprograms(\n$subprograms\n)"
    }
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