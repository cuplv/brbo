package brbo.common

import brbo.common.BeforeOrAfterOrThis.{AFTER, BeforeOrAfterOrThis}
import com.sun.source.tree.{StatementTree, VariableTree}

/**
 *
 * @param predicate     The ASTs that satisfy the given predicate
 * @param beforeOrAfter The location before or after the ASTs that satisfy the condition
 */
case class Locations(predicate: StatementTree => Boolean, beforeOrAfter: BeforeOrAfterOrThis)

object Locations {
  def afterAllCommands: Locations = {
    Locations(
      {
        case _: VariableTree => false // To ensure `D` is declared before each `assert(D==D)`
        case tree if TreeUtils.isCommand(tree) => true
        case _ => false
      },
      AFTER
    )
  }
}