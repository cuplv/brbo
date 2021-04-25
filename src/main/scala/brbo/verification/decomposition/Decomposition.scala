package brbo.verification.decomposition

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common.TypeUtils.BrboType
import brbo.common._
import brbo.common.instrument.FileFormat.JAVA_FORMAT
import brbo.common.instrument.InstrumentUtils
import brbo.common.instrument.InstrumentUtils.NewMethodInformation
import brbo.verification.AmortizationMode.{FULL_AMORTIZE, NO_AMORTIZE, SELECTIVE_AMORTIZE}
import brbo.verification.BasicProcessor
import com.sun.source.tree._

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

class Decomposition(inputMethod: TargetMethod, arguments: CommandLineArguments) extends DecompositionInterface(inputMethod, arguments) {

  override def decompose: List[DecompositionResult] = decompose(fullAmortize, selectiveAmortize, noAmortize)

  def noAmortize: IntermediateResult[Subprogram] = {
    logger.info(s"Decompose mode: `${brbo.verification.AmortizationMode.NO_AMORTIZE}`")
    val subprograms = commands.foldLeft(new HashSet[Subprogram])({
      (acc, command) =>
        command match {
          case expressionStatementTree: ExpressionStatementTree =>
            GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource) match {
              case Some(_) => acc + Subprogram(inputMethod, List(command))
              case None => acc
            }
          case _ => acc
        }
    })
    IntermediateResult(Groups(subprograms), NO_AMORTIZE)
  }

  def fullAmortize: IntermediateResult[Subprogram] = {
    logger.info(s"Decompose mode: `${brbo.verification.AmortizationMode.FULL_AMORTIZE}`")
    val subprogram = Subprogram(inputMethod, inputMethod.methodTree.getBody.getStatements.asScala.toList)
    IntermediateResult(Groups(HashSet[Subprogram](subprogram)), FULL_AMORTIZE)
  }

  def selectiveAmortize: IntermediateResult[Subprogram] = {
    logger.info(s"Decompose mode: `${brbo.verification.AmortizationMode.SELECTIVE_AMORTIZE}`")
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
            logger.info(s"Enlarge a subprogram due to interference between subprograms")
            traceOrError(s"Enlarge subprogram: ${pair._1}")
            enlarge(pair._1)
          }
          else {
            logger.info(s"Merge a subprogram due to interference between subprograms")
            traceOrError(s"Merge subprograms: $pair")
            merge(pair._1, pair._2)
          }
        }
        traceOrError(s"Decomposition - New subprogram: $newSubprogram")
        subprograms = mergeIfOverlap(subprograms.programs - pair._1 - pair._2 + newSubprogram)
      }
    }
    IntermediateResult(Groups(subprograms.programs), SELECTIVE_AMORTIZE)
  }

  def initializeSubprograms(): Set[Subprogram] = {
    commands.foldLeft(new HashSet[Subprogram])({
      (acc, statement) =>
        DecompositionUtils.initializeGroups(statement, inputMethod) match {
          case Some((statement, _)) => acc + Subprogram(inputMethod, List(statement))
          case None => acc
        }
    })
  }

  def merge(subprogram1: Subprogram, subprogram2: Subprogram): Subprogram = {
    traceOrError(s"Merge - Subprogram 1: $subprogram1\nSubprogram 2: $subprogram2")
    (subprogram1.minimalEnclosingBlock, subprogram2.minimalEnclosingBlock) match {
      case (Some(minimalEnclosingBlock1), Some(minimalEnclosingBlock2)) if minimalEnclosingBlock1 == minimalEnclosingBlock2 =>
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
        traceOrError(s"Merge - Choose a subsequence from index `$head` to `$last` in minimal enclosing block `$minimalEnclosingBlock1`")
        return Subprogram(inputMethod, minimalEnclosingBlock1.getStatements.asScala.slice(head, last + 1).toList)
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
            traceOrError(s"Merge - Choose a subsequence from index `$startIndex` to `$endIndex` in common enclosing block `$blockTree`")
            return Subprogram(inputMethod, statements.slice(startIndex, endIndex + 1))
          case tree@_ => return Subprogram(inputMethod, List(tree.asInstanceOf[StatementTree]))
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
          logger.info(s"Enlarge a subprogram due to interference from the environment")
          traceOrError(s"Subprogram is interfered by the environment: $interferedSubprogram")
          val newSubprogram: Subprogram = enlarge(interferedSubprogram)
          traceOrError(s"Eliminate environment interference - New subprogram: $newSubprogram")
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
      traceOrError(s"Enlarge - Contain dangling `break` or `continue`? $containsBreakContinue")
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
          case indexHead =>
            if (indexHead == 0) {
              statements.indexOf(subprogram.astNodes.last) match {
                case -1 => throw new Exception(s"Unexpected")
                case indexLast =>
                  if (indexLast == statements.size() - 1) {
                    List(inputMethod.getPath(headAstNode).getParentPath.getLeaf)
                  }
                  else {
                    val newStatement = statements.get(indexLast + 1)
                    traceOrError(s"Enlarge - (Next) New statement $newStatement")
                    avoidDanglingBreakContinue(newStatement, minimalEnclosingBlock) match {
                      case Some(minimalLoop) => List(minimalLoop)
                      case None => subprogram.astNodes :+ newStatement
                    }
                  }
              }
            }
            else {
              val newStatement = statements.get(indexHead - 1)
              traceOrError(s"Enlarging - (Previous) New statement $newStatement")
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

    Subprogram(inputMethod, newAstNodes.map(tree => tree.asInstanceOf[StatementTree]))
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
          logger.info(s"Merge two subprograms because they overlap with each other")
          traceOrError(s"Merge if overlap - Subprogram 1: ${pair._1}\nSubprogram 2: ${pair._2}\nNew subprogram: $newSubprogram")
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
        traceOrError(s"Maximal enclosing loop: $maximalLoop")
        val environmentCommands = TreeUtils.collectCommands(maximalLoop.asInstanceOf[StatementTree]).filter({
          command =>
            !subprograms.programs.exists({ subprogram => subprogram.commands.contains(command) })
        })
        traceOrError(s"Environment commands: $environmentCommands")
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
    traceOrError(s"Environment modified set: `$modifiedSet`")

    val taintSet = DecompositionUtils.controlDataDependencyForResources(subprogram.targetMethod, debug)
    traceOrError(s"Taint set `$taintSet` of subprogram\n$subprogram")

    modifiedSet.intersect(taintSet.inputs).nonEmpty
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
          if (subprogram1.innerTrees.subsetOf(allTrees) && subprogram2.innerTrees.subsetOf(allTrees)) {
            allCommonEnclosingTrees = statementTree :: allCommonEnclosingTrees
          }
        case _ => continue = false
      }
      p = p.getParentPath
    }
    allCommonEnclosingTrees
  }

  def subsequentExecute(subprogram1: Subprogram, subprogram2: Subprogram): Boolean = {
    val commonTrees = getAllCommonEnclosingTrees(subprogram1, subprogram2)
    if (commonTrees.exists(tree => TreeUtils.loopKinds.contains(tree.getKind)))
      true
    else {
      // Find the maximal block tree that encloses both
      commonTrees.find(tree => tree.isInstanceOf[BlockTree]) match {
        case Some(blockTree) =>
          val statements = blockTree.asInstanceOf[BlockTree].getStatements.asScala
          val trees = statements.map(statement => TreeUtils.collectStatementTrees(statement)).toList.zipWithIndex
          val index1 = {
            var list = List[Int]()
            trees.foreach({
              case (tree, index) => if (subprogram1.innerTrees.intersect(tree).nonEmpty) list = index :: list
            })
            list.head
          }
          val index2 = {
            var list = List[Int]()
            trees.foreach({
              case (tree, index) => if (subprogram2.innerTrees.intersect(tree).nonEmpty) list = index :: list
            })
            list.last
          }
          assert(index1 != -1, s"Block tree: $blockTree\nSubprogram 1: $subprogram1")
          assert(index2 != -1, s"Block tree: $blockTree\nSubprogram 2: $subprogram2")
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
    if (!subsequentExecute(subprogram1, subprogram2)) {
      traceOrError(s"Subprogram 2 cannot be subsequently executed after subprogram 1")
      traceOrError(s"Subprogram 1:\n$subprogram1")
      traceOrError(s"Subprogram 2:\n$subprogram2")
      return false
    }

    val modifiedSet1 = DecompositionUtils.computeModifiedSet(subprogram1.targetMethod)
    val taintSet2 = DecompositionUtils.controlDataDependencyForResources(subprogram2.targetMethod, debug)
    traceOrError(s"Subprogram 1 modified set: $modifiedSet1")
    traceOrError(s"Subprogram 1:\n$subprogram1")
    traceOrError(s"Subprogram 2 taint set: $taintSet2")
    traceOrError(s"Subprogram 2:\n$subprogram2")
    modifiedSet1.intersect(taintSet2.inputs).nonEmpty
  }
}

case class Subprogram(inputMethod: TargetMethod, astNodes: List[StatementTree]) extends Segment {
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
    org.checkerframework.javacutil.TreePathUtil.enclosingOfKind(inputMethod.getPath(astNodes.head), Tree.Kind.BLOCK) match {
      case null => throw new Exception("Unexpected")
      case blockTree: BlockTree =>
        if (blockTree.getStatements.contains(astNodes.head)) Some(blockTree)
        else None
    }
  }
  assert(minimalEnclosingBlock.isEmpty || astNodes.size == 1 || minimalEnclosingTree == minimalEnclosingBlock.get)
  assert(minimalEnclosingBlock.nonEmpty || minimalEnclosingTree == astNodes.head)

  private val declaredLocalVariables = {
    innerTrees.foldLeft(new HashSet[String])({
      (acc, tree) =>
        tree match {
          case variableTree: VariableTree => acc + variableTree.getName.toString
          case _ => acc
        }
    }).toList.sortWith(_ < _)
  }

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
      NewMethodInformation(Some(parameters), None, None, List("import brbo.benchmarks.Common;"), Some("Common"), isAbstractClass = true, newMethodBody = s"{\n$methodBody\n}"),
      JAVA_FORMAT,
      indent = 2
    )
  }

  val targetMethod: TargetMethod = BasicProcessor.getTargetMethod(inputMethod.fullQualifiedClassName, javaProgramRepresentation)

  override def containCommand(tree: StatementTree): Boolean = {
    assert(TreeUtils.isCommand(tree))
    commands.contains(tree)
  }

  override def beginCommand: StatementTree = astNodes.head
}

case class Subprograms(programs: Set[Subprogram]) {
  // Any two subprograms should not overlap with each other
  /*MathUtils.crossJoin(List(programs, programs)).foreach({
    programs2 =>
      val program1 = programs2.head
      val program2 = programs2.tail.head
      if (program1 != program2) assert(!overlap(program1, program2), s"Overlapping subprograms:\n$program1\n$program2")
  })*/

  override def toString: String = {
    val subprograms = programs.map(x => x.toString).toList.mkString("\n")
    s"Subprograms(\n$subprograms\n)"
  }
}