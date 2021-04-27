package brbo.verification.decomposition

import brbo.common._
import brbo.common.cfg.{CFGUtils, UniqueNode}
import brbo.verification.AmortizationMode.{FULL_AMORTIZE, NO_AMORTIZE, SELECTIVE_AMORTIZE}
import brbo.verification.dependency.{DependencyAnalysis, Dominator}
import com.sun.source.tree.StatementTree
import org.checkerframework.dataflow.cfg.node.Node

import scala.collection.immutable.HashSet

class NewDecomposition(inputMethod: TargetMethod, arguments: CommandLineArguments, testMode: Boolean) extends DecompositionInterface(inputMethod, arguments) {
  private val reachingDefinition = inputMethod.reachingDefinitions
  private val controlDependency = inputMethod.controlDependency
  private val dominator = new Dominator(inputMethod)

  override protected val debug: Boolean = false

  override def decompose: List[DecompositionResult] = decompose(fullAmortize, selectiveAmortize, noAmortize)

  def selectiveAmortize: IntermediateResult[Group] = {
    val groups = initializeGroups()
    val newGroups = mergeGroups(groups)
    val finalGroups = Groups(newGroups.elements.map(group => decideReset(group)))
    IntermediateResult(finalGroups, SELECTIVE_AMORTIZE)
  }

  def noAmortize: IntermediateResult[Group] = {
    val groups = initializeGroups()
    val newGroups = groups.elements.map(group => Group(Some(group.updates.head.statement), group.updates))
    IntermediateResult(Groups(newGroups), NO_AMORTIZE)
  }

  def fullAmortize: IntermediateResult[Group] = {
    val updates = {
      sortedCommands.flatMap({ statement => initializeGroups(statement, inputMethod) })
        .map(pair => Update(pair._1, pair._2))
    }
    val firstCommand = TreeUtils.findFirstCommand(inputMethod.methodTree)
    assert(firstCommand.isDefined)
    val group = Group(firstCommand, updates)
    IntermediateResult(Groups(Set(group)), FULL_AMORTIZE)
  }

  def initializeGroups(): Groups[Group] = {
    val updateCommands = sortedCommands.foldLeft(new HashSet[Group])({
      (acc, statement) =>
        initializeGroups(statement, inputMethod) match {
          case Some((statement, node)) => acc + Group(None, List(Update(statement, node)))
          case None => acc
        }
    })
    Groups(updateCommands)
  }

  def mergeGroups(groups: Groups[Group]): Groups[Group] = {
    var newGroups: Set[Group] = groups.elements
    var continue = true
    while (continue) {
      MathUtils.crossJoin2(newGroups, newGroups)
        .filter(pair => pair._1 != pair._2)
        .find({ pair => shouldMerge(pair._1, pair._2) }) match {
        case Some(pair) =>
          val newGroup = Group(None, pair._1.updates ++ pair._2.updates)
          logger.info(s"Merge two groups")
          traceOrError(s"Group 1: ${pair._1}\nGroup 2: ${pair._2}\nNew group: $newGroup")
          newGroups = newGroups - pair._1 - pair._2 + newGroup
        case None => continue = false
      }
    }
    Groups(newGroups)
  }

  def shouldMerge(group1: Group, group2: Group): Boolean = {
    val taintSet1 = group1.taintSetsDataOnly // group1.taintSets.flatMap(taintSet => taintSet.allVariables -- taintSet.inputs)
    val taintSet2 = group2.taintSetsDataOnly // group2.taintSets.flatMap(taintSet => taintSet.allVariables -- taintSet.inputs)
    traceOrError(s"Group 1: $group1")
    traceOrError(s"Group 1 taint set: $taintSet1")
    traceOrError(s"Group 2: $group2")
    traceOrError(s"Group 2 taint set: $taintSet2")
    taintSet1.allVariables.intersect(taintSet2.allVariables).nonEmpty
  }

  def decideReset(group: Group): Group = {
    logger.info(s"Decide reset for: ${group.updates.map(u => s"${u.statement} (${u.statement.hashCode()})")}")
    val updateNodes = group.updates.map(u => UniqueNode(u.node)) // Two Nodes can be .equals but represent different CFG nodes

    val allResets = inputMethod.commandsNodesMap.filter({
      case (candidateReset, resetNodes) =>
        // For each command in the input program that dominates all update commands in the group, construct a new program
        if (resetNodes.isEmpty) {
          false
        }
        else {
          traceOrError(s"Try reset `$candidateReset`")
          val dominate = MathUtils.crossJoin2(resetNodes, updateNodes).forall({
            case (resetNode, updateNode) =>
              val result = dominator.isDominatedBy(updateNode, resetNode)
              val not = if (!result) "not" else ""
              traceOrError(s"${CFGUtils.nodeUniqueIdentifier(updateNode.node)} is $not dominated by ${CFGUtils.nodeUniqueIdentifier(resetNode.node)}")
              result
          })

          if (!dominate) {
            false
          }
          else {
            /*val newMethod = ChangeEntryNode.changeEntryNode(inputMethod, candidateReset, group.updates.map(u => u.statement).toSet, testMode)
            val taintSet = {
              val taintSet = DependencyAnalysis.controlDataDependencyForResources(newMethod, debug = false)
              TaintSet.removeResourceVariables(taintSet)
            }
            logger.trace(s"Command: `$candidateReset`. Inputs: ${taintSet.inputs}. Program:\n${newMethod.methodTree}")
            taintSet.inputs.forall(identifier => inputMethod.inputVariables.contains(identifier))*/

            logger.error(s"Command `$candidateReset` dominates all updates")
            true
          }
        }
    }).toList.sortWith({ // This sort is stable
      case (pair1, pair2) =>
        val sortedNodes1 = UniqueNode.sortNodes(pair1._2)
        val sortedNodes2 = UniqueNode.sortNodes(pair2._2)
        s"${pair1._1}: $sortedNodes1" < s"${pair2._2}: $sortedNodes2"
    }).sortWith({
      case (pair1, pair2) =>
        val resetNodes1 = pair1._2
        val resetNodes2 = pair2._2
        resetNodes1.forall({
          resetNode1 =>
            resetNodes2.forall(resetNode2 => dominator.isDominatedBy(resetNode1, resetNode2))
        })
    })

    val resetCommand: StatementTree = {
      if (allResets.isEmpty) throw new Exception("Unexpected")
      else allResets.head._1
    }

    logger.info(s"Decide reset at `$resetCommand (${resetCommand.hashCode()})` for: ${group.updates.map(u => s"${u.statement} (${u.statement.hashCode()})")}")
    Group(Some(resetCommand), group.updates)
  }

  case class Update(statement: StatementTree, node: Node)

  case class Group(resetLocation: Option[StatementTree], updates: List[Update]) extends Segment {
    val taintSets: TaintSet = {
      val taintSets = updates.map({
        update =>
          val taintSet = DependencyAnalysis.taintSetPerExecution(update.node, reachingDefinition, controlDependency, debug = false)
          TaintSet.removeResourceVariables(taintSet)
      })
      TaintSet.merge(taintSets)
    }
    val taintSetsDataOnly: TaintSet = {
      val taintSets = updates.map({
        update =>
          val taintSet = DependencyAnalysis.transitiveDataDependency(update.node, reachingDefinition, Set(), excludeResourceVariables = true, debug = false)
          traceOrError(s"Command `${update.statement}` -> Taint sets (data only): ${taintSet.toTestString}")
          TaintSet.removeResourceVariables(taintSet)
      })
      TaintSet.merge(taintSets)
    }

    resetLocation match {
      case Some(statement) => assert(TreeUtils.isCommand(statement))
      case None =>
    }

    override def toTestString: String = s"Group(${resetLocation.toString}, ${updates.map(u => u.toString).sorted})"

    override def beginCommand: StatementTree = {
      resetLocation match {
        case Some(value) => value
        case None => throw new Exception("Unexpected")
      }
    }

    override def containCommand(tree: StatementTree): Boolean = updates.exists(u => u.statement == tree)
  }
}
