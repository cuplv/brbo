package brbo.verification

import brbo.common.GhostVariableUtils.GhostVariable.{Counter, Delta, Resource}
import brbo.common.InstrumentUtils.FileFormat.JAVA_FORMAT
import brbo.common.TypeUtils.BrboType
import brbo.common._
import brbo.verification.Decomposition.DecompositionResult
import brbo.verification.dependency.reachdef.ReachingValue
import brbo.verification.dependency.{ControlDependency, DataDependency}
import com.sun.source.tree.Tree.Kind
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.block.{Block, ConditionalBlock}
import org.checkerframework.dataflow.cfg.node._

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

class Decomposition(inputMethod: TargetMethod) {
  private val logger = LogManager.getLogger(classOf[Decomposition])

  private val commands = TreeUtils.collectCommands(inputMethod.methodTree.getBody)

  private val debug = true

  def debugOrError(message: String): Unit = {
    if (debug) logger.error(message)
    else logger.debug(message)
  }

  def decompose(): Subprograms = {
    var subprograms = eliminateEnvironmentInterference(mergeIfOverlap(initializeSubprograms()))
    var continue = true
    while (continue) {
      val interferedSubprograms = findInterference(subprograms)
      if (interferedSubprograms.isEmpty) {
        continue = false
      }
      else {
        val pair = interferedSubprograms.head
        val newSubprogram = {
          if (pair._1 == pair._2) {
            debugOrError(s"Enlarging subprogram: ${pair._1}")
            enlarge(pair._1)
          }
          else {
            debugOrError(s"Merging subprograms: $pair")
            merge(pair._1, pair._2)
          }
        }
        debugOrError(s"Decomposition - New subprogram: $newSubprogram")
        subprograms = mergeIfOverlap(subprograms.programs - pair._1 - pair._2 + newSubprogram)
      }
    }
    subprograms
  }

  def initializeSubprograms(): Set[Subprogram] = {
    commands.foldLeft(new HashSet[Subprogram])({
      (acc, statement) =>
        Decomposition.initializeSubprogramFromStatement(statement, inputMethod) match {
          case Some((statement, _)) => acc + Subprogram(List(statement))
          case None => acc
        }
    })
  }

  def insertGhostVariables(subprograms: Subprograms): DecompositionResult = {
    val deltaVariables = {
      subprograms.programs.zipWithIndex
    }
    ???
  }

  /**
   *
   * @param subprogram The subprogram to be enlarged
   * @return A new subprogram such that
   *         - It encloses the input subprogram
   *         - It does not overlap with other subprograms
   *         - No interference: It does not interfere with itself
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
        getAllCommonEnclosingTrees(subprogram1, subprogram2).last match {
          case blockTree: BlockTree =>
            val statements = blockTree.getStatements.asScala.toList
            val statements2 = statements.filter({
              statement =>
                val trees = TreeUtils.collectStatementTrees(statement)
                trees.intersect(subprogram1.innerTrees).nonEmpty || trees.intersect(subprogram2.innerTrees).nonEmpty
            })
            val startIndex = statements.indexOf(statements2.head)
            val endIndex = statements.indexOf(statements2.last)
            assert(startIndex != -1 && endIndex != -1)
            return Subprogram(statements.slice(startIndex, endIndex + 1))
          case tree@_ => return Subprogram(List(tree.asInstanceOf[StatementTree]))
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
          debugOrError(s"Subprogram is interfered by the environment: $interferedSubprogram")
          val newSubprogram: Subprogram = enlarge(interferedSubprogram)
          debugOrError(s"Eliminate environment interference - New subprogram: $newSubprogram")
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
    def avoidDanglingBreakContinue(newStatement: StatementTree, minimalEnclosingBlock: Tree): Option[Tree] = {
      val trees = TreeUtils.collectStatementTrees(newStatement)
      val containsBreakContinue = {
        TreeUtils.collectCommands(newStatement).exists({
          case tree@(_: BreakTree | _: ContinueTree) =>
            TreeUtils.getMinimalEnclosingLoop(inputMethod.getPath(tree)) match {
              case Some(enclosingLoop) =>
                !TreeUtils.collectStatementTrees(enclosingLoop.asInstanceOf[StatementTree]).subsetOf(trees)
              case None => true
            }
          case _ => false
        })
      }
      debugOrError(s"Enlarging - Containing dangling `break` or `continue`? $containsBreakContinue")
      if (containsBreakContinue) {
        val minimalLoop = TreeUtils.getMinimalEnclosingLoop(inputMethod.getPath(minimalEnclosingBlock))
        assert(minimalLoop.isDefined)
        minimalLoop
      }
      else None
    }

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
                    val newStatement = statements.get(index + 1)
                    debugOrError(s"Enlarging - (Next) New statement $newStatement")
                    avoidDanglingBreakContinue(newStatement, minimalEnclosingBlock) match {
                      case Some(minimalLoop) => List(minimalLoop)
                      case None => subprogram.astNodes :+ newStatement
                    }
                  }
              }
            }
            else {
              val newStatement = statements.get(index - 1)
              debugOrError(s"Enlarging - (Previous) New statement $newStatement")
              avoidDanglingBreakContinue(newStatement, minimalEnclosingBlock) match {
                case Some(minimalLoop) => List(minimalLoop)
                case None => newStatement :: subprogram.astNodes
              }
            }
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

  def environmentModifiedSet(subprogram: Subprogram, subprograms: Subprograms): Set[String] = {
    // Environment interferes only if there exists a path from the subprgram to the environment
    TreeUtils.getMaximalEnclosingLoop(inputMethod.getPath(subprogram.astNodes.head)) match {
      case Some(maximalLoop) =>
        logger.debug(s"Maximal enclosing loop: $maximalLoop")
        val environmentCommands = TreeUtils.collectCommands(maximalLoop.asInstanceOf[StatementTree]).filter({
          command =>
            !subprograms.programs.exists({ subprogram => subprogram.commands.contains(command) })
        })
        logger.debug(s"Environment commands: $environmentCommands")
        environmentCommands
          .flatMap(statement => TreeUtils.modifiedVariables(statement))
          .toSet
          .filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource))
      case None => new HashSet[String]
    }
  }

  def interferedByEnvironment(subprogram: Subprogram, subprograms: Subprograms): Boolean = {
    assert(subprograms.programs.contains(subprogram))

    val modifiedSet = environmentModifiedSet(subprogram, subprograms)
    logger.debug(s"Environment modified set: `$modifiedSet`")

    val taintSet = Decomposition.computeTaintSet(subprogram.targetMethod, debug = false)
    logger.debug(s"Taint set `$taintSet` of subprogram\n$subprogram")

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

  def getAllCommonEnclosingTrees(subprogram1: Subprogram, subprogram2: Subprogram): List[StatementTree] = {
    var allCommonEnclosingTrees: List[StatementTree] = Nil
    var continue = true
    var p = inputMethod.getPath(subprogram1.astNodes.head)
    while (p != null && continue) {
      val leaf = p.getLeaf
      assert(leaf != null)
      leaf match {
        case statementTree: StatementTree =>
          val allTrees = TreeUtils.collectStatementTrees(statementTree)
          if (subprogram2.innerTrees.subsetOf(allTrees)) {
            allCommonEnclosingTrees = statementTree :: allCommonEnclosingTrees
          }
        case _ => continue = false
      }
      p = p.getParentPath
    }
    allCommonEnclosingTrees
  }

  def subsequentExecution(subprogram1: Subprogram, subprogram2: Subprogram): Boolean = {
    val commonTrees = getAllCommonEnclosingTrees(subprogram1, subprogram2)
    if (commonTrees.exists(tree => TreeUtils.loopKinds.contains(tree.getKind))) {
      true
    }
    else {
      commonTrees.find(tree => tree.isInstanceOf[BlockTree]) match {
        case Some(blockTree) =>
          val statements = blockTree.asInstanceOf[BlockTree].getStatements.asScala
          val trees = statements.map(statement => TreeUtils.collectStatementTrees(statement))
          val index1 = trees.indexWhere(tree => subprogram1.innerTrees.subsetOf(tree))
          val index2 = trees.indexWhere(tree => subprogram2.innerTrees.subsetOf(tree))
          assert(index1 != -1)
          assert(index2 != -1)
          index1 < index2
        case None => throw new Exception("Unexpected")
      }
    }
  }

  /**
   *
   * @param subprogram1 Subprogram 1
   * @param subprogram2 Subprogram 2
   * @return If subprogram 1 may interfere with subprogram 2's resource usage
   */
  def interfere(subprogram1: Subprogram, subprogram2: Subprogram): Boolean = {
    // First check if there exists a path from subprogram1's exit to subprogram2's entry
    if (!subsequentExecution(subprogram1, subprogram2)) {
      debugOrError(s"Subprogram 2 cannot be subsequently executed after subprogram 1")
      debugOrError(s"Subprogram 1:\n$subprogram1")
      debugOrError(s"Subprogram 2:\n$subprogram2")
      return false
    }

    val modifiedSet1 = Decomposition.computeModifiedSet(subprogram1.targetMethod)
    val taintSet2 = Decomposition.computeTaintSet(subprogram2.targetMethod, debug = false)
    debugOrError(s"Subprogram 1 modified set: $modifiedSet1")
    debugOrError(s"Subprogram 1:\n$subprogram1")
    debugOrError(s"Subprogram 2 taint set: $taintSet2")
    debugOrError(s"Subprogram 2:\n$subprogram2")
    modifiedSet1.intersect(taintSet2).nonEmpty
  }

  case class Subprogram(astNodes: List[StatementTree]) {
    assert(astNodes.nonEmpty)

    override def toString: String = {
      val nodes = astNodes.map(t => t.toString).mkString("\n")
      s"Subprogram(\n$nodes\n)"
    }

    val innerTrees: Set[StatementTree] = astNodes.flatMap({ astNode => TreeUtils.collectStatementTrees(astNode) }).toSet
    val commands: List[StatementTree] = TreeUtils.collectCommands(astNodes)

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
      val methodBody = astNodes.map(tree => s"${tree.toString};").mkString("\n")
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
  private val logger = LogManager.getLogger("brbo.verification.Decomposition")

  def debugOrError(message: String, debug: Boolean): Unit = {
    if (debug) logger.error(message)
    else logger.debug(message)
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
        GhostVariableUtils.extractGhostVariableUpdate(expressionStatementTree.getExpression, Resource) match {
          case Some(updateTree) =>
            val updateNode = CFGUtils.getNodesCorrespondingToExpressionStatementTree(statement, targetMethod.cfg)

            updateTree.increment match {
              case literalTree: LiteralTree =>
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
                Some(subprogram, entryNode)
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
            debugOrError(s"Compute taint set for `$initialSubprogram`", debug)
            val conditionTrees = TreeUtils.collectConditionTreesWithoutBrackets(initialSubprogram)

            val correspondingNode = CFGUtils.getNodesCorrespondingToExpressionStatementTree(statement, targetMethod.cfg)
            debugOrError(s"Expression Tree `$statement` corresponds to node `$correspondingNode`", debug)

            // Treat `r+=e` when `e` is constant differently from `r+=e` when `e` is constant
            val dataDependentVariables: Set[String] = GhostVariableUtils.extractGhostVariableUpdate(correspondingNode, Resource) match {
              case Some(update) =>
                update.increment match {
                  case updateNode: ValueLiteralNode =>
                    debugOrError(s"The update in `$statement` is constant", debug)
                    updateNode match {
                      case _: IntegerLiteralNode =>
                        computeTransitiveControlDependency(correspondingNode, controlDependency, new HashSet[Node], debug).flatMap({
                          conditionNode =>
                            val conditionTree = conditionNode.getTree.asInstanceOf[ExpressionTree]
                            debugOrError(s"The condition node is `$conditionNode`", debug)
                            logger.trace(s"The condition tree is `$conditionTree` (hash code: ${conditionTree.hashCode()})")
                            conditionTrees.foreach(t => logger.trace(s"Condition tree `$t` (hash code: ${t.hashCode()})"))
                            if (conditionTrees.contains(conditionTree)) {
                              CFGUtils.getUsedVariables(conditionNode)
                            }
                            else new HashSet[String]
                        })
                      case _ => throw new Exception(s"Unsupported literal node `$updateNode`")
                    }
                  case _ =>
                    debugOrError(s"The update in statement `$statement` is not constant", debug)
                    CFGUtils.getUsedVariables(correspondingNode.asInstanceOf[AssignmentNode].getExpression)
                }
              case None => throw new Exception("Unexpected")
            }
            debugOrError(s"Data dependent variables are $dataDependentVariables", debug)

            // Compute transitive data dependency for the above variables, starting from the entry node of initial subprogram
            computeTransitiveDataDependencyInputsOnly(dataDependentVariables, entryNode, dataDependency, debug)
          case None => new HashSet[String]
        }
    })
    taintSet.filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource)).toSet
  }

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

  private def computeTransitiveDataDependencyInputsOnlyHelper(node: Node,
                                                              dataDependency: Map[Node, Set[ReachingValue]],
                                                              visited: Set[Node],
                                                              debug: Boolean): Set[String] = {
    if (visited.contains(node)) return new HashSet[String]
    debugOrError(s"Compute data dependency for node `$node`", debug)

    val usedVariables: Set[String] = node match {
      case assignmentNode: AssignmentNode => CFGUtils.getUsedVariables(assignmentNode.getExpression)
      case methodInvocationNode: MethodInvocationNode =>
        methodInvocationNode.getArguments.asScala.flatMap(node => CFGUtils.getUsedVariables(node)).toSet
      case _: VariableDeclarationNode => new HashSet[String]
      case _ => throw new Exception(s"Expecting assignment or method invocation node `$node` (Type: `${node.getClass}`)")
    }
    debugOrError(s"Used variables: $usedVariables", debug)

    dataDependency.get(node) match {
      case Some(definitions) =>
        val newDefinitions = definitions.filter(definition => usedVariables.contains(definition.variable))
        debugOrError(s"Reaching definitions: $newDefinitions", debug)
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

  private def computeTransitiveControlDependency(node: Node,
                                                 controlDependency: Map[Block, Set[Block]],
                                                 visited: Set[Node],
                                                 debug: Boolean): Set[Node] = {
    if (visited.contains(node)) return new HashSet[Node]
    debugOrError(s"Visiting node `$node`", debug)

    controlDependency.get(node.getBlock) match {
      case Some(blocks) =>
        blocks.flatMap({
          block =>
            assert(block.isInstanceOf[ConditionalBlock], s"Block ${node.getBlock.getUid} control depends on block ${block.getUid}")
            val predecessors = block.getPredecessors.asScala
            assert(predecessors.size == 1)

            val condition = predecessors.head.getLastNode
            computeTransitiveControlDependency(condition, controlDependency, visited + node, debug) + condition
        })
      case None => new HashSet[Node]
    }
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

  @deprecated
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

  @deprecated
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

  @deprecated
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

  case class DeltaCounterPair(delta: String, counter: String) {
    GhostVariableUtils.isGhostVariable(delta, Delta)
    GhostVariableUtils.isGhostVariable(counter, Counter)
  }

  case class DecompositionResult(className: String, sourceFileContents: String, deltaCounterPairs: Set[DeltaCounterPair]) {
    val targetMethod: TargetMethod = BasicProcessor.getTargetMethod(className, sourceFileContents)
  }

}