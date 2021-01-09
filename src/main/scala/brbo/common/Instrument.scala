package brbo.common

import brbo.boundinference.BasicProcessor
import brbo.common.InstrumentUtils.AtomicStatementInstrumentation
import brbo.common.InstrumentUtils.InstrumentMode.InstrumentMode
import org.apache.logging.log4j.LogManager

object Instrument {
  class InstrumentProcessor extends BasicProcessor {
    private val logger = LogManager.getLogger(classOf[InstrumentProcessor])
  }

  /**
   *
   * @param input                          A Java class that defines a Java method
   * @param instrumentMode                 Whether to instrument at most one atomic statement or all atomic statement
   * @param atomicStatementInstrumentation How to update atomic statements
   * @return A Java class that defines a Java method
   */
  def instrument(input: String, instrumentMode: InstrumentMode, atomicStatementInstrumentation: AtomicStatementInstrumentation): String = {
    ???
  }
}
