package brbo.common.javac

import com.sun.tools.javac.main.JavaCompiler
import com.sun.tools.javac.util.{Context, List, Options}
import org.apache.logging.log4j.LogManager
import org.checkerframework.dataflow.cfg.{CFGProcessor, ControlFlowGraph}

import java.io.ByteArrayOutputStream
import java.net.URI
import java.util
import javax.annotation.processing.Processor
import javax.tools.JavaFileObject.Kind
import javax.tools.SimpleJavaFileObject

object JavacUtils {
  private val logger = LogManager.getLogger("brbo.common.JavacUtils")

  val commonClassQualifiedName = "brbo.benchmarks.Common"

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
    // val bytesErr = new ByteArrayOutputStream()
    // val bytesOut = new ByteArrayOutputStream()
    // val oldOutStream = System.out
    // val oldErrStream = System.err
    // val newErrSteam = new PrintStream(bytesErr, true, "UTF-8")
    // val newOutSteam = new PrintStream(bytesOut, true, "UTF-8")
    try {
      // Redirect syserr
      // System.setErr(newErrSteam)
      // System.setOut(newOutSteam)
      val fileObject = new JavaSourceFromString(compilationUnitName, sourceCode)
      javac.compile(createJavaList(fileObject), createJavaList(compilationUnitName), createJavaList(processor), createJavaList())

      /*val javac2 = ToolProvider.getSystemJavaCompiler
      val diagnostics: DiagnosticCollector[JavaFileObject] = new DiagnosticCollector[JavaFileObject]
      val fileManager: StandardJavaFileManager = javac2.getStandardFileManager(diagnostics, null, null)

      val compilationUnit = java.util.Arrays.asList(fileObject)
      val clazz = java.util.Arrays.asList(s"$compilationUnitName")
      val task = javac2.getTask(null, fileManager, diagnostics, null, clazz, compilationUnit)
      task.setProcessors(java.util.Arrays.asList(processor))
      task.call

      diagnostics.getDiagnostics.asScala.foreach({
        diagnostic =>
          logger.fatal(s"Compilation error: ${diagnostic.toString}")
      })*/
    }
    catch {
      case _: EarlyStopException =>
        logger.trace(s"Early stop when running processor ${processor.toString}")
      case e: Throwable =>
        // logger.fatal(s"Exception in running processor ${processor.toString} for source code:\n$sourceCode", e)
        // s"${bytesErr.toString}\n${bytesOut.toString}"
        throw e
    }
    finally {
      // System.setErr(oldErrStream)
      // System.setOut(oldOutStream)
    }
  }

  private def createJavaList[T](elements: T*): java.util.List[T] = {
    val list = new util.LinkedList[T]()
    elements.foreach(element => list.add(element))
    list
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
