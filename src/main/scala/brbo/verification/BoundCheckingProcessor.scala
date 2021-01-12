package brbo.verification

import javax.annotation.processing.SupportedAnnotationTypes
import org.apache.logging.log4j.LogManager

@SupportedAnnotationTypes(Array("*"))
class BoundCheckingProcessor extends BasicProcessor {
  private val logger = LogManager.getLogger(classOf[BoundCheckingProcessor])

  validateInputProgram()

  /**
   * The input program of this processor should:
   * 1. Include 1 reset for each delta variable
   * 2. Include >=1 updates for each delta variable
   * 3. Include 1 update for each counter
   * 4. There is no `assert(e)`
   * 5. There exists 1 resource variable
   */
  def validateInputProgram(): Unit = {
  }
}
