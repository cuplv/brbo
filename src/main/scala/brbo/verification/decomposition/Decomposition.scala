package brbo.verification.decomposition

import brbo.common.BeforeOrAfterOrThis.BEFORE
import brbo.common.GhostVariableUtils.GhostVariable.{Delta, Resource}
import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.{BrboType, INT}
import brbo.common._
import brbo.common.instrument.FileFormat.JAVA_FORMAT
import brbo.common.instrument.InstrumentUtils.{NewMethodInformation, appendSemiColon}
import brbo.common.instrument.{InstrumentUtils, StatementTreeInstrumentation}
import brbo.verification.AmortizationMode.{AmortizationMode, FULL_AMORTIZE, NO_AMORTIZE, SELECTIVE_AMORTIZE}
import brbo.verification.BasicProcessor
import com.sun.source.tree._

import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}

class Decomposition(inputMethod: TargetMethod, arguments: CommandLineArguments) extends DecompositionInterface(inputMethod, arguments) {

  override def decompose(): List[DecompositionResult] = {
    val listOfSubprograms: List[IntermediateResult] = {
      val amortizationMode = arguments.getAmortizationMode
      logger.info(s"Decomposing... Mode: `$amortizationMode`")
      amortizationMode match {
        case brbo.verification.AmortizationMode.NO_AMORTIZE =>
          List[IntermediateResult](decomposeNoAmortize())
        case brbo.verification.AmortizationMode.FULL_AMORTIZE =>
          List[IntermediateResult](decomposeFullAmortize())
        case brbo.verification.AmortizationMode.SELECTIVE_AMORTIZE =>
          List[IntermediateResult](decomposeSelectiveAmortize())
        case brbo.verification.AmortizationMode.ALL_AMORTIZE =>
          List[IntermediateResult](
            decomposeNoAmortize(),
            decomposeSelectiveAmortize(),
            decomposeFullAmortize()
          )
      }
    }
    listOfSubprograms.map({ result => insertGhostVariables(result) })
  }

  def decomposeNoAmortize(): IntermediateResult = {
    logger.info(s"Decompose mode: `${brbo.verification.AmortizationMode.NO_AMORTIZE}`")
    val subprograms = commands.foldLeft(new HashSet[Subprogram])({
      (acc, command) =>
        command match {
          case expressionStatementTree: ExpressionStatementTree =>
            GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource) match {
              case Some(_) => acc + Subprogram(List(command))
              case None => acc
            }
          case _ => acc
        }
    })
    IntermediateResult(Subprograms(subprograms), NO_AMORTIZE)
  }

  def decomposeFullAmortize(): IntermediateResult = {
    logger.info(s"Decompose mode: `${brbo.verification.AmortizationMode.FULL_AMORTIZE}`")
    val subprogram = Subprogram(inputMethod.methodTree.getBody.getStatements.asScala.toList)
    IntermediateResult(Subprograms(HashSet[Subprogram](subprogram)), FULL_AMORTIZE)
  }

  def decomposeSelectiveAmortize(): IntermediateResult = {
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
    IntermediateResult(subprograms, SELECTIVE_AMORTIZE)
  }

  def initializeSubprograms(): Set[Subprogram] = {
    commands.foldLeft(new HashSet[Subprogram])({
      (acc, statement) =>
        DecompositionUtils.initializeGroups(statement, inputMethod) match {
          case Some((statement, _)) => acc + Subprogram(List(statement))
          case None => acc
        }
    })
  }

  private def shouldInstrument(subprograms: Subprograms)(tree: StatementTree): Boolean = {
    if (tree == null) return false

    val insertResetsAndCounterUpdates = subprograms.programs.exists({
      subprogram => subprogram.astNodes.head == tree
    })

    val insertUpdates = {
      if (TreeUtils.isCommand(tree)) {
        tree match {
          case expressionStatementTree: ExpressionStatementTree =>
            GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource).isDefined
          case _ => false
        }
      }
      else false
    }

    insertResetsAndCounterUpdates || insertUpdates
  }

  private def whatToInsert(deltaCounterPairs: Map[Subprogram, DeltaCounterPair])(tree: StatementTree): String = {
    if (tree == null) return ""

    val subprograms = deltaCounterPairs.keySet
    val prepend1: String = subprograms.find({
      subprogram => subprogram.astNodes.head == tree
    }) match {
      case Some(subprogram) =>
        val pair = deltaCounterPairs(subprogram)
        val deltaPrime = GhostVariableUtils.generateDeltaVariablePrime(pair.delta)
        // s"$deltaPrime = ($deltaPrime > ${pair.delta}) ? $deltaPrime: ${pair.delta}; ${pair.delta} = 0; ${pair.counter} = ${pair.counter} + 1;"
        s"$deltaPrime = ${pair.delta}; ${pair.delta} = 0; ${pair.counter} = ${pair.counter} + 1;"
      case None => ""
    }

    val prepend2: String = {
      if (TreeUtils.isCommand(tree)) {
        tree match {
          case expressionStatementTree: ExpressionStatementTree =>
            GhostVariableUtils.extractUpdate(expressionStatementTree.getExpression, Resource) match {
              case Some(updateTree) =>
                // Assume there is only 1 subprogram that contains command `R=R+e`
                subprograms.find(subprogram => subprogram.commands.contains(tree)) match {
                  case Some(subprogram) =>
                    val pair = deltaCounterPairs(subprogram)
                    s"${pair.delta} = ${pair.delta} + ${updateTree.increment}"
                  case None => ""
                }
              case None => ""
            }
          case _ => ""
        }
      }
      else ""
    }

    s"${appendSemiColon(prepend1)}${appendSemiColon(prepend2)}"
  }

  def insertGhostVariables(intermediateResult: IntermediateResult): DecompositionResult = {
    val subprograms: Subprograms = intermediateResult.subprograms
    val amortizationMode: AmortizationMode = intermediateResult.amortizationMode
    logger.info(s"Inserting resets and updates to ghost variables... Mode: `$amortizationMode`")

    val deltaCounterPairs: Map[Subprogram, DeltaCounterPair] = {
      val deltaVariables: Map[Subprogram, String] = {
        // Eliminate non-deterministic behavior
        val indexedSubprograms: List[(Subprogram, Int)] = subprograms.programs.toList.sortWith({
          case (subprogram1, subprogram2) => subprogram1.toString < subprogram2.toString
        }).zipWithIndex
        indexedSubprograms.foldLeft(new HashMap[Subprogram, String])({
          case (acc, (subprogram, index)) =>
            acc + (subprogram -> GhostVariableUtils.generateName(index.toString, Delta))
        })
      }
      subprograms.programs.foldLeft(new HashMap[Subprogram, DeltaCounterPair])({
        (acc, subprogram) =>
          val pair = DeltaCounterPair(deltaVariables(subprogram), counterMap(subprogram.astNodes.head))
          acc + (subprogram -> pair)
      })
    }

    val newMethodBody = {
      val ghostVariableDeclaration = {
        val ghostVariables = deltaCounterPairs.values.foldLeft(new HashMap[String, BrboType])({
          (acc, pair) => acc + (pair.delta -> INT) + (GhostVariableUtils.generateDeltaVariablePrime(pair.delta) -> INT) + (pair.counter -> INT)
        })
        val spaces = " " * 4
        val declarations = ghostVariables
          .map(pair => BrboType.variableDeclarationAndInitialization(pair._1, pair._2, JAVA_FORMAT))
          .toList.sortWith(_ < _).mkString(s"\n$spaces")
        s"$spaces$declarations"
      }
      val newMethodBody = InstrumentUtils.instrumentStatementTrees(
        inputMethod,
        StatementTreeInstrumentation(
          Locations(shouldInstrument(subprograms), BEFORE),
          whatToInsert(deltaCounterPairs)
        ),
        indent = 2
      )
      // TODO: A very hacky way to insert variable declarations
      newMethodBody.replaceFirst("""\{""", s"{\n$ghostVariableDeclaration")
    }
    val newSourceFile = InstrumentUtils.replaceMethodBodyAndGenerateSourceCode(
      inputMethod,
      NewMethodInformation(None, None, None, List("import brbo.benchmarks.Common;"), Some("Common"), isAbstractClass = true, newMethodBody),
      JAVA_FORMAT,
      indent = 2
    )
    val result = DecompositionResult(newSourceFile, deltaCounterPairs.values.toSet, amortizationMode, inputMethod)
    if (debug || arguments.getPrintCFG) CFGUtils.printPDF(result.outputMethod.cfg, Some("decomposed-"))
    result
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
            traceOrError(s"Merge - Choose a subsequence from index `$startIndex` to `$endIndex` in common enclosing block `$blockTree`")
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

    val taintSet = DecompositionUtils.computeTaintSetControlAndData(subprogram.targetMethod, debug)
    traceOrError(s"Taint set `$taintSet` of subprogram\n$subprogram")

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
    val taintSet2 = DecompositionUtils.computeTaintSetControlAndData(subprogram2.targetMethod, debug)
    traceOrError(s"Subprogram 1 modified set: $modifiedSet1")
    traceOrError(s"Subprogram 1:\n$subprogram1")
    traceOrError(s"Subprogram 2 taint set: $taintSet2")
    traceOrError(s"Subprogram 2:\n$subprogram2")
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

  case class IntermediateResult(subprograms: Subprograms, amortizationMode: AmortizationMode)

}