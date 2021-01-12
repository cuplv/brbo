package brbo.verification

import brbo.common.GhostVariableUtils.GhostVariable.Counter
import brbo.common.{GhostVariableUtils, Z3Solver}
import com.microsoft.z3.AST
import com.sun.source.tree._
import org.apache.logging.log4j.LogManager

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

object CounterAxiomGenerator {
  private val logger = LogManager.getLogger("brbo.boundinference.CounterAxiomGenerator")

  private val FIRST_COUNTER_ID = 0
  val FIRST_COUNTER_NAME: String = generateCounterId(FIRST_COUNTER_ID)

  def generateCounterId(id: Int): String = GhostVariableUtils.generateGhostVariable(id.toString, Counter)

  /**
   *
   * @param tree The AST that we wish to generate a counter mapping
   * @return A mapping from all ASTs of the AST to their unique counters
   */
  def generateCounterMap(tree: Tree): Map[Tree, String] = {
    logger.info(s"Attaching unique counters to AST nodes")
    generateCounterMapHelper(tree, FIRST_COUNTER_ID)._1
  }

  def generateCounterMapHelper(tree: Tree, id: Int): (Map[Tree, String], Int) = {
    def throwException(message: String): Nothing = {
      throw new Exception(s"Generate counter map - $message in AST: $tree")
    }

    var map = new HashMap[Tree, String]
    var newId = id

    tree match {
      case _@(_: AssertTree | _: BreakTree | _: ContinueTree | _: EmptyStatementTree |
              _: ExpressionStatementTree | _: ReturnTree | _: VariableTree) =>
        map = map + (tree -> generateCounterId(id))
        newId = newId + 1
      case tree: BlockTree =>
        map = map + (tree -> generateCounterId(id))
        newId = newId + 1

        tree.getStatements.asScala.foreach({
          statement =>
            val (newMap, newNewId) = generateCounterMapHelper(statement, newId)
            map = map ++ newMap
            newId = newNewId
        })
      case _: ClassTree => throwException("Unexpected class tree")
      case tree: DoWhileLoopTree =>
        map = map + (tree -> generateCounterId(id))
        newId = newId + 1

        val (newMap, newNewId) = generateCounterMapHelper(tree.getStatement, newId)
        map = map ++ newMap
        newId = newNewId
      case _: EnhancedForLoopTree => throwException("Not yet support enhanced for loop")
      case tree: ForLoopTree =>
        map = map + (tree -> generateCounterId(id))
        newId = newId + 1

        val initializers = tree.getInitializer.asScala
        val updates = tree.getUpdate.asScala
        assert(initializers.size <= 1)
        assert(updates.size <= 1)
        val (newMap1, newNewId1) = generateCounterMapHelper(initializers.head, newId)
        map = map ++ newMap1
        newId = newNewId1
        val (newMap2, newNewId2) = generateCounterMapHelper(tree.getStatement, newId)
        map = map ++ newMap2
        newId = newNewId2
        val (newMap3, newNewId3) = generateCounterMapHelper(updates.head, newId)
        map = map ++ newMap3
        newId = newNewId3
      case tree: IfTree =>
        map = map + (tree -> generateCounterId(id))
        newId = newId + 1

        val (newMap1, newNewId1) = generateCounterMapHelper(tree.getThenStatement, newId)
        map = map ++ newMap1
        newId = newNewId1
        val (newMap2, newNewId2) = generateCounterMapHelper(tree.getElseStatement, newId)
        map = map ++ newMap2
        newId = newNewId2
      case tree: LabeledStatementTree =>
        map = map + (tree -> generateCounterId(id))
        newId = newId + 1

        val (newMap, newNewId) = generateCounterMapHelper(tree.getStatement, newId)
        map = map ++ newMap
        newId = newNewId
      case _: SwitchTree => throwException("Not yet support switch tree")
      case _: SynchronizedTree => throwException("Not yet support synchronized tree")
      case _: ThrowTree => throwException("Not yet support throw tree")
      case _: TryTree => throwException("Not yet support try tree")
      case tree: WhileLoopTree =>
        map = map + (tree -> generateCounterId(id))
        newId = newId + 1

        val (newMap, newNewId) = generateCounterMapHelper(tree.getStatement, newId)
        map = map ++ newMap
        newId = newNewId
    }
    (map, newId)
  }

  /**
   *
   * @param solver     The solver for generating Z3 ASTs
   * @param methodBody The program that we wish to generate counter axioms for
   * @return Counter axioms
   */
  def generateCounterAxioms(solver: Z3Solver, methodBody: Tree): AST = {
    logger.info(s"Generating counter axioms")
    generateCounterAxiomsHelper(solver, methodBody, generateCounterMap(methodBody))
  }

  private def generateCounterAxiomsHelper(solver: Z3Solver, tree: Tree, counterMap: Map[Tree, String]): AST = {
    def throwException(message: String): Nothing = {
      throw new Exception(s"Generate counter axioms - $message in AST: $tree")
    }

    tree match {
      case _@(_: AssertTree | _: BreakTree | _: ContinueTree | _: EmptyStatementTree |
              _: ExpressionStatementTree | _: ReturnTree | _: VariableTree) =>
        solver.mkTrue()
      case tree: BlockTree =>
        val statements: List[Tree] = tree.getStatements.asScala.toList
        if (statements.isEmpty)
          solver.mkTrue()
        else {
          val innerPredicates = solver.mkAnd(statements.map(tree => generateCounterAxiomsHelper(solver, tree, counterMap)): _*)

          val counters1 = (tree :: statements.dropRight(1)).map(tree => solver.mkIntVar(counterMap(tree)))
          val counters2 = statements.map(tree => solver.mkIntVar(counterMap(tree)))
          val predicates = counters1.zip(counters2).map({
            case (counter1, counter2) =>
              solver.mkAnd(
                solver.mkGe(solver.mkAdd(counter2, solver.mkIntVal(1)), counter1),
                solver.mkGe(counter1, counter2)
              )
          })
          solver.mkAnd(solver.mkAnd(predicates: _*), innerPredicates)
        }
      case _: ClassTree => throwException("Unexpected class tree")
      case tree: DoWhileLoopTree =>
        val counter0 = solver.mkIntVar(counterMap(tree))
        val counter1 = solver.mkIntVar(counterMap(tree.getStatement))
        solver.mkAnd(
          solver.mkGe(counter1, counter0),
          generateCounterAxiomsHelper(solver, tree.getStatement, counterMap)
        )
      case _: EnhancedForLoopTree => throwException("Not yet support enhanced for loop")
      case tree: ForLoopTree =>
        val initializers = tree.getInitializer.asScala
        val updates = tree.getUpdate.asScala
        assert(initializers.size <= 1)
        assert(updates.size <= 1)
        val counter0 = solver.mkIntVar(counterMap(tree))
        val predicate1 = {
          if (initializers.isEmpty)
            solver.mkTrue()
          else {
            val counter1 = solver.mkIntVar(counterMap(initializers.head))
            solver.mkAnd(
              solver.mkGe(solver.mkAdd(counter1, solver.mkIntVal(1)), counter0),
              solver.mkGe(counter0, counter1),
              generateCounterAxiomsHelper(solver, initializers.head, counterMap)
            )
          }
        }
        val counter2 = solver.mkIntVar(counterMap(tree.getStatement))
        val predicate2 = {
          if (updates.isEmpty)
            solver.mkTrue()
          else {
            val counter3 = solver.mkIntVar(counterMap(updates.head))
            solver.mkAnd(
              solver.mkGe(solver.mkAdd(counter3, solver.mkIntVal(1)), counter2),
              solver.mkGe(counter2, counter3),
              generateCounterAxiomsHelper(solver, updates.head, counterMap)
            )
          }
        }
        solver.mkAnd(
          predicate1,
          predicate2,
          generateCounterAxiomsHelper(solver, tree.getStatement, counterMap)
        )
      case tree: IfTree =>
        val counter0 = solver.mkIntVar(counterMap(tree))
        val counter1 = solver.mkIntVar(counterMap(tree.getThenStatement))
        val counter2 = solver.mkIntVar(counterMap(tree.getElseStatement))
        solver.mkAnd(
          solver.mkGe(solver.mkAdd(counter1, counter2, solver.mkIntVal(1)), counter0),
          solver.mkGe(counter0, solver.mkAdd(counter1, counter2)),
          generateCounterAxiomsHelper(solver, tree.getThenStatement, counterMap),
          generateCounterAxiomsHelper(solver, tree.getElseStatement, counterMap)
        )
      case tree: LabeledStatementTree =>
        val counter0 = solver.mkIntVar(counterMap(tree))
        val counter1 = solver.mkIntVar(counterMap(tree.getStatement))
        solver.mkAnd(
          solver.mkEq(counter0, counter1),
          generateCounterAxiomsHelper(solver, tree.getStatement, counterMap)
        )
      case _: SwitchTree => throwException("Not yet support switch tree")
      case _: SynchronizedTree => throwException("Not yet support synchronized tree")
      case _: ThrowTree => throwException("Not yet support throw tree")
      case _: TryTree => throwException("Not yet support try tree")
      case tree: WhileLoopTree =>
        generateCounterAxiomsHelper(solver, tree.getStatement, counterMap)
    }
  }
}
