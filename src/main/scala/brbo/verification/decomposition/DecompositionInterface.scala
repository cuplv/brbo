package brbo.verification.decomposition

import brbo.common.{CommandLineArguments, TargetMethod}
import brbo.verification.CounterAxiomGenerator
import com.sun.source.tree.{StatementTree, Tree}
import org.apache.logging.log4j.{LogManager, Logger}

abstract class DecompositionInterface(inputMethod: TargetMethod, arguments: CommandLineArguments) {
  protected val debug: Boolean = arguments.getDebugMode
  protected val logger: Logger = LogManager.getLogger(classOf[Decomposition])

  protected val commands: List[StatementTree] = inputMethod.commands

  protected val counterMap: Map[Tree, String] = CounterAxiomGenerator.generateCounterMap(inputMethod.methodTree.getBody)

  protected def traceOrError(message: String): Unit = {
    if (debug) logger.error(message)
    else logger.trace(message)
  }

  def decompose(): List[DecompositionResult]
}
