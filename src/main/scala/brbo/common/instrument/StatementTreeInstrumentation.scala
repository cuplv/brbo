package brbo.common.instrument

import brbo.common.Locations
import com.sun.source.tree.StatementTree

case class StatementTreeInstrumentation(locations: Locations, whatToInsert: StatementTree => String)
