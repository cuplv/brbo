package brbo.benchmarks

import brbo.common.GhostVariableUtils
import brbo.common.GhostVariableUtils.GhostVariable.Resource

sealed trait AST {
  def toString(indent: Int): String
}

case class Statement(statements: List[BasicStatement]) extends AST {
  override def toString(indent: Int): String = {
    statements.map(b => b.toString(indent)).mkString("\n")
  }
}

sealed trait BasicStatement extends AST

case class Loop(header: Header, statement: Statement) extends BasicStatement {
  override def toString(indent: Int): String = {
    val spaces = " " * indent
    s"$spaces${header.toString(indent)} {\n" + statement.toString(indent + GenerateSyntheticPrograms.INDENT) + s"\n$spaces}"
  }
}

case class Command(resourceVariable: String, update: Symbol) extends BasicStatement {
  assert(GhostVariableUtils.isGhostVariable(resourceVariable, Resource))

  override def toString(indent: Int): String = {
    val spaces = " " * indent
    s"$spaces$resourceVariable = $resourceVariable + ${update.toString(0)};"
  }
}

sealed trait Header extends AST

case class BasicHeader(i: String, upperBound: String) extends Header {
  override def toString(indent: Int): String = {
    s"for (int $i = 0; $i < $upperBound; i++)"
  }
}

case class IteratorHeader(iterator: String, initialValue: String, entry: String) extends Header {
  override def toString(indent: Int): String = {
    val update = s"$entry = ndInt2(1, $iterator)"
    s"for (int $iterator = $initialValue, $update; $iterator > 0; $iterator -= $entry, $update)"
  }
}

case class Symbol(value: Either[Int, String]) extends AST {
  override def toString(indent: Int): String = {
    assert(indent == 0)
    value match {
      case Left(value) => value.toString
      case Right(value) => value
    }
  }
}