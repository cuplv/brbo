package brbo.common

import java.io.{IOException, OutputStream, PrintStream}
import java.net.URI

import brbo.boundinference.EarlyStopException
import com.sun.tools.javac.main.JavaCompiler
import com.sun.tools.javac.util.{Context, List, Options}
import javax.annotation.processing.Processor
import javax.tools.JavaFileObject.Kind
import javax.tools.SimpleJavaFileObject
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.ControlFlowGraph
import org.checkerframework.dataflow.cfg.CFGProcessor

object JavacUtils {
  private val logger = LogManager.getLogger("brbo.common.JavacUtils")

  def runCFGProcessor(className: String, methodName: String, sourceFileName: String, sourceCode: String): ControlFlowGraph = {
    val cfgProcessor = new CFGProcessor(className, methodName)
    runProcessor(sourceFileName, sourceCode, cfgProcessor)
    val res = cfgProcessor.getCFGProcessResult
    if (res == null) {
      logger.error("Internal error in type processor! method typeProcessOver() doesn't get called.")
      System.exit(1)
    }

    if (!res.isSuccess) {
      logger.error(res.getErrMsg)
      System.exit(1)
    }
    res.getCFG
  }

  def runProcessor(compilationUnitName: String, sourceCode: String, processor: Processor): Unit = {
    val context = new Context
    Options.instance(context).put("compilePolicy", "ATTR_ONLY")
    val javac = new JavaCompiler(context)
    try {
      // redirect syserr to nothing (and prevent the compiler from issuing
      // warnings about our exception.
      System.setErr(new PrintStream(new OutputStream() {
        @throws[IOException]
        def write(b: Int): Unit = {}
      }))
      val fileObject = new JavaSourceFromString(compilationUnitName, sourceCode)
      javac.compile(List.of(fileObject), List.of(compilationUnitName), List.of(processor))
    }
    catch {
      case _: EarlyStopException => logger.debug(s"Early stop when running processor ${processor.toString}")
      case e: Throwable => logger.error(s"Exception in running processor ${processor.toString}", e)
    }
    finally {
      System.setErr(System.err)
    }
  }

  /**
   * A file object used to represent source coming from a string.
   */
  class JavaSourceFromString(val name: String,

                             /**
                              * The source code of this "file".
                              */
                             val code: String)

  /**
   * Constructs a new JavaSourceFromString.
   *
   * @param name the name of the compilation unit represented by this file object
   * @param code the source code for the compilation unit represented by this file object
   */
    extends SimpleJavaFileObject(URI.create("string:///" + name.replace('.', '/') + Kind.SOURCE.`extension`), Kind.SOURCE) {
    override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = code
  }
  // https://docs.oracle.com/javase/8/docs/api/javax/tools/JavaCompiler.html
}