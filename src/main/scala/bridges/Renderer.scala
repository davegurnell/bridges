package bridges

import org.apache.commons.lang3.StringEscapeUtils.{escapeJava => escape}

trait Renderer[A] {
  def render(decls: List[Declaration[_]]): String
}

trait BaseRenderer[A] extends Renderer[A] {
  import Type._

  def render(decls: List[Declaration[_]]): String =
    decls.map(render).mkString("\n\n")

  def render(decl: Declaration[_]): String =
    s"export type ${decl.id} = ${renderType(decl.tpe)};"

  def renderType(tpe: Type): String =
    tpe match {
      case Ref(id)             => id
      case StrLiteral(str)     => "\"" + escape(str) + "\""
      case NumLiteral(num)     => num.toString
      case BoolLiteral(bool)   => bool.toString
      case Str                 => "string"
      case Num                 => "number"
      case Bool                => "boolean"
      case Null                => "null"
      case Array(tpe)          => "Array<" + renderType(tpe) + ">"
      case Struct(fields)      => fields.map(renderField).mkString("{ ", ", ", " }")
      case Union(types)        => types.map(renderType).mkString("(", " | ", ")")
      case Intersection(types) => types.map(renderType).mkString("(", " & ", ")")
    }

  def renderField(field: (String, Type)): String =
    s"""${field._1}: ${renderType(field._2)}"""
}

object Renderer {
  implicit object TypescriptRenderer extends BaseRenderer[Typescript]
  implicit object FlowRenderer extends BaseRenderer[Flow]
}
