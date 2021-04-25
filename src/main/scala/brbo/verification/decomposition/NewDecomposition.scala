package brbo.verification.decomposition

import brbo.common.{CommandLineArguments, MathUtils, TargetMethod}
import brbo.verification.dependency.{ControlDependency, DataDependency}
import com.sun.source.tree.StatementTree
import org.checkerframework.dataflow.cfg.node.Node

import scala.collection.immutable.HashSet

class NewDecomposition(inputMethod: TargetMethod, arguments: CommandLineArguments) extends DecompositionInterface(inputMethod, arguments) {
  private val dataDependency = DataDependency.computeDataDependency(inputMethod)
  private val controlDependency = {
    val controlDependency = ControlDependency.computeControlDependency(inputMethod)
    ControlDependency.reverseControlDependency(controlDependency)
  }

  override def decompose(): List[DecompositionResult] = {
    val groups = initializeGroups()
    val newGroups = mergeGroups(groups)
    val finalGroups = Groups(newGroups.elements.map(group => Group(Some(decideReset(group)), group.updates)))
    ???
  }

  def initializeGroups(): Groups = {
    val updateCommands = commands.foldLeft(new HashSet[Group])({
      (acc, statement) =>
        DecompositionUtils.initializeGroups(statement, inputMethod) match {
          case Some((statement, node)) => acc + Group(None, HashSet(Update(statement, node)))
          case None => acc
        }
    })
    Groups(updateCommands)
  }

  def mergeGroups(groups: Groups): Groups = {
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

  def decideReset(group: Group): StatementTree = {
    ???
  }

  case class Update(statement: StatementTree, node: Node)

  case class Group(resetLocation: Option[StatementTree], updates: Set[Update]) {
    val taintSets: Set[TaintSet] = updates.map({
      update => DecompositionUtils.taintSetPerExecution(update.node, dataDependency, controlDependency, new HashSet[Node], debug = false)
    })
  }

  case class Groups(elements: Set[Group])

}
