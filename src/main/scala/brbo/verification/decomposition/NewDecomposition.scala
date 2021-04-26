package brbo.verification.decomposition

import brbo.common.GhostVariableUtils.GhostVariable.Resource
import brbo.common._
import brbo.common.instrument.ChangeEntryNode
import brbo.verification.AmortizationMode.{NO_AMORTIZE, FULL_AMORTIZE, SELECTIVE_AMORTIZE}
import brbo.verification.dependency.{ControlDependency, Dominator, ReachingDefinition}
import com.sun.source.tree.{BreakTree, ContinueTree, StatementTree}
import org.checkerframework.dataflow.cfg.node.Node

import scala.collection.immutable.HashSet

class NewDecomposition(inputMethod: TargetMethod, arguments: CommandLineArguments) extends DecompositionInterface(inputMethod, arguments) {
  private val reachingDefinition = ReachingDefinition.run(inputMethod)
  private val controlDependency = {
    val controlDependency = ControlDependency.computeControlDependency(inputMethod)
    ControlDependency.reverseControlDependency(controlDependency)
  }
  private val dominator = new Dominator(inputMethod)

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
      commands.flatMap({ statement => initializeGroups(statement, inputMethod) })
        .map(pair => Update(pair._1, pair._2))
    }
    val firstCommand = inputMethod.commands.last
    val group = Group(Some(firstCommand), updates)
    IntermediateResult(Groups(Set(group)), FULL_AMORTIZE)
  }

  def initializeGroups(): Groups[Group] = {
    val updateCommands = commands.foldLeft(new HashSet[Group])({
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
    val taintSet1 = group1.taintSets.flatMap(taintSet => taintSet.allVariables -- taintSet.inputs)
    val taintSet2 = group2.taintSets.flatMap(taintSet => taintSet.allVariables -- taintSet.inputs)
    traceOrError(s"Group 1: $group1")
    traceOrError(s"Group 1 taint set: $taintSet1")
    traceOrError(s"Group 2: $group2")
    traceOrError(s"Group 2 taint set: $taintSet2")
    taintSet1.intersect(taintSet2).nonEmpty
  }

  def decideReset(group: Group): Group = {
    val updateNodes = group.updates.map(u => u.node) // Two Nodes can be .equals but represent different CFG nodes

    inputMethod.commands.find({
      command =>
        // For each command in the input program that dominates all update commands in the group, construct a new program
        val resetNodes = TreeUtils.getNodesCorrespondingToCommand(inputMethod.cfg, command)
        if (resetNodes.isEmpty) false
        else {
          val dominate = resetNodes.forall({
            resetNode => updateNodes.forall(updateNode => dominator.isDominatedBy(updateNode, resetNode))
          })

          if (!dominate) false
          else if (!command.isInstanceOf[BreakTree] && !command.isInstanceOf[ContinueTree]) {
            val newMethod = ChangeEntryNode.changeEntryNode(inputMethod, command, group.updates.map(u => u.statement).toSet, testMode = false)
            val taintSet = DecompositionUtils.controlDataDependencyForResources(newMethod, debug = false)
            logger.trace(s"Command: $command. Inputs: ${taintSet.inputs}. Program:\n${newMethod.methodTree}")
            taintSet.inputs
              .filter(identifier => !GhostVariableUtils.isGhostVariable(identifier, Resource))
              .forall(identifier => inputMethod.inputVariables.contains(identifier))
          }
          else false
        }
    }) match {
      case Some(resetCommand) =>
        logger.info(s"Decide reset at `$resetCommand` for: ${group.updates.map(u => u.statement)}")
        Group(Some(resetCommand), group.updates)
      case None => throw new Exception("Unexpected")
    }
  }

  case class Update(statement: StatementTree, node: Node)

  case class Group(resetLocation: Option[StatementTree], updates: List[Update]) extends Segment {
    val taintSets: List[TaintSet] = updates.map({
      update => DecompositionUtils.taintSetPerExecution(update.node, reachingDefinition, controlDependency, new HashSet[Node], debug = false)
    })

    resetLocation match {
      case Some(statement) => assert(TreeUtils.isCommand(statement))
      case None =>
    }

    override def toTestString: String = s"Group(${resetLocation.toString}, ${updates.toList.map(u => u.toString).sorted})"

    override def beginCommand: StatementTree = {
      resetLocation match {
        case Some(value) => value
        case None => throw new Exception("Unexpected")
      }
    }

    override def containCommand(tree: StatementTree): Boolean = updates.exists(u => u.statement == tree)
  }

}
