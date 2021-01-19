package brbo.common

import brbo.common.BeforeOrAfter.BeforeOrAfter
import org.checkerframework.dataflow.cfg.node.Node

/**
 *
 * @param whichASTs     Insert `assert(1)` before / after Which ASTs. We use `Node` instead of `Tree`
 *                      because we are using `Node` as intermediate representations that are less
 *                      "syntactic" than ASTs
 * @param beforeOrAfter Insert `assert(1)` either before or after the ASTs that satisfy the condition
 */
case class Locations(whichASTs: Node => Boolean, beforeOrAfter: BeforeOrAfter)
