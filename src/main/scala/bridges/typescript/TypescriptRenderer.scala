package bridges.typescript

import bridges.core._
import bridges.core.Type._
import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }

trait TypescriptRenderer {

  def render(decl: Declaration): String =
    s"export type ${decl.id} = ${renderType(decl.tpe)};"

  private def renderType(tpe: Type): String =
    tpe match {
      case Ref(id)          => id
      case Str              => "string"
      case Character        => "string"
      case Num              => "number"
      case Floating         => "number"
      case Bool             => "boolean"
      case Optional(optTpe) => "(" + renderType(optTpe) + " | null)"
      case Array(arrTpe)    => renderType(arrTpe) + "[]"
      case Struct(fields)   => fields.map(renderField).mkString("{ ", ", ", " }")
//      case Union(types)           => types.map(renderType).mkString("(", " | ", ")")
      // Typescript languages don't care about the fields when building Union types as they act as references to a type defined somewhere else
//      case Intersection(key, iTpe, _) => List(key, iTpe).map(renderType).mkString("(", " & ", ")")
      //TODO: fix and remove default case
      case _ ⇒ ""
    }

  private def renderField(field: (String, Type)): String =
    s"""${field._1}: ${renderType(field._2)}"""
}