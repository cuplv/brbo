package brbo.common

import brbo.common.BeforeOrAfterOrThis.BeforeOrAfterOrThis
import com.sun.source.tree.StatementTree

/**
 *
 * @param predicate     The ASTs that satisfy the given predicate
 * @param beforeOrAfter The location before or after the ASTs that satisfy the condition
 */
case class Locations(predicate: StatementTree => Boolean, beforeOrAfter: BeforeOrAfterOrThis)
