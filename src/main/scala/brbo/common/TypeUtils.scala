package brbo.common

import brbo.common.InstrumentUtils.FileFormat.{C_FORMAT, FileFormat, JAVA_FORMAT}
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import javax.lang.model.`type`.TypeMirror
import org.checkerframework.framework.`type`.AnnotatedTypeMirror

import scala.collection.immutable.HashMap

object TypeUtils {

  object BrboType extends Enumeration {
    type BrboType = Value
    val INT, BOOL, VOID = Value

    def toString(typ: BrboType, fileFormat: FileFormat): String = {
      typ match {
        case INT => "int"
        case BOOL =>
          fileFormat match {
            case JAVA_FORMAT => "boolean"
            case C_FORMAT => "int"
          }
        case VOID => "void"
      }
    }

    def variableDeclaration(identifier: String, typ: BrboType): String = {
      s"${BrboType.toString(typ, JAVA_FORMAT)} $identifier"
    }

    def variableDeclarationAndInitialization(identifier: String, typ: BrboType, fileFormat: FileFormat): String = {
      val initialValue = typ match {
        case INT => 0
        case BOOL =>
          fileFormat match {
            case JAVA_FORMAT => true
            case C_FORMAT => 0
          }
      }
      s"${BrboType.toString(typ, JAVA_FORMAT)} $identifier = $initialValue;"
    }
  }

  def typeTranslation(typ: TypeMirror): BrboType = {
    val defaultTyp = INT
    typ.toString match {
      case "int" | "java.lang.Integer" => INT
      case "boolean" | "java.lang.Boolean" => BOOL
      case _ => defaultTyp
    }
  }

  def typeTranslation(anno: AnnotatedTypeMirror): BrboType = typeTranslation(anno.getUnderlyingType)

  def typeMapTranslation(map: Map[String, TypeMirror]): Map[String, BrboType] = {
    map.foldLeft(HashMap[String, BrboType]())({
      case (acc, (name, typ)) => acc + (name -> typeTranslation(typ))
    })
  }
}
