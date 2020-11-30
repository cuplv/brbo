package brbo.common

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import javax.lang.model.`type`.TypeMirror
import org.checkerframework.framework.`type`.AnnotatedTypeMirror

import scala.collection.immutable.HashMap

object TypeUtils {
  object BrboType extends Enumeration {
    type BrboType = Value
    val INT, BOOL = Value
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
