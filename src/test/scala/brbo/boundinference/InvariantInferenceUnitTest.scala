package brbo.boundinference

import brbo.TestCase
import brbo.boundinference.BeforeOrAfter._
import brbo.common.InstrumentUtils.GhostVariable.Delta
import brbo.common.TypeUtils.BrboType.{BrboType, INT}
import brbo.common.{InstrumentUtils, JavacUtils, Z3Solver}
import org.checkerframework.dataflow.cfg.node.{AssignmentNode, Node}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.{HashMap, HashSet}

class InvariantInferenceUnitTest extends AnyFlatSpec {
  "Invariant inference for delta variables should succeed" should "?" in {
    InvariantInferenceUnitTest.deltaVariableTests.foreach({
      testCase =>
        val solver = new Z3Solver
        val invariantInferenceProcessor = new InvariantInferenceProcessor(solver)
        JavacUtils.runProcessor(testCase.name, testCase.input, invariantInferenceProcessor)
        val result = invariantInferenceProcessor.inferInvariant(
          Locations(
            {
              node: Node =>
                InstrumentUtils.extractGhostVariableFromAssignment(node, Delta) match {
                  case Some(_) => true
                  case None => false
                }
            },
            AFTER
          ),
          HashMap[String, BrboType](
            "D100" -> INT,
            "R" -> INT,
            "i" -> INT,
            "C1" -> INT
          )
        )
        println(result)
    })
  }
}

object InvariantInferenceUnitTest {
  val deltaVariableTests: HashSet[TestCase] = {
    val test01 =
      """class Test01 {
        |  void f(int n)
        |  {
        |    int D100 = 0;
        |    int C1 = 0;
        |    int R = 0;
        |    int i = 0;
        |    D100 = D100 + 1;
        |    while (i < n)
        |    {
        |      i++;
        |      C1 = C1 + 1;
        |      D100 = D100 + 1;
        |      R = R + 1;
        |    }
        |  }
        |}""".stripMargin
    val test01Expected = ""

    HashSet[TestCase](
      TestCase("Test01", test01, test01Expected),
      // TestCase("Test02", test02, test02Expected),
      // TestCase("Test03", test03, test03Expected),
      // TestCase("Test04", test04, test04Expected)
    )
  }
}