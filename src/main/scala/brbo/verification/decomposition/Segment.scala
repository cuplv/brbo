package brbo.verification.decomposition

import com.sun.source.tree.StatementTree

abstract class Segment {
  def beginCommand: StatementTree

  def containCommand(tree: StatementTree): Boolean
}
