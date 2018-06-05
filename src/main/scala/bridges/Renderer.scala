package bridges

import org.apache.commons.lang3.StringEscapeUtils.{escapeJava => escape}

trait Renderer[A] {
  def render(decls: List[Declaration]): String
}

trait TypescriptStyleRenderer[A] extends Renderer[A] {
  import Type._

  def render(decls: List[Declaration]): String =
    decls.map(render).mkString("\n\n")

  def render(decl: Declaration): String =
    s"export type ${decl.id} = ${renderType(decl.tpe)};"

  def renderType(tpe: Type): String =
    tpe match {
      case Ref(id)             => id
      case StrLiteral(str)     => "\"" + escape(str) + "\""
      case CharLiteral(ch)     => "\"" + ch + "\""
      case NumLiteral(num)     => num.toString
      case FloatingLiteral(fl) => fl.toString
      case BoolLiteral(bool)   => bool.toString
      case Str                 => "string"
      case Character           => "string"
      case Num                 => "number"
      case Floating            => "number"
      case Bool                => "boolean"
      case Optional(optTpe)    => "(" + renderType(optTpe) + " | null)"
      case Array(arrTpe)       => "Array<" + renderType(arrTpe) + ">"
      case Struct(fields)      => fields.map(renderField).mkString("{ ", ", ", " }")
      case Union(types)        => types.map(renderType).mkString("(", " | ", ")")
      case Intersection(types) =>
        types.map(renderType).mkString("(", " & ", ")")
    }

  def renderField(field: (String, Type)): String =
    s"""${field._1}: ${renderType(field._2)}"""
}

trait ElmStyleRenderer[A] extends Renderer[A] {
  import Type._

  def render(decls: List[Declaration]): String =
    decls.map(render).mkString("\n\n")

  def render(decl: Declaration): String =
    decl.tpe match {
      case Union(types) ⇒
        s"type ${decl.id} = ${types.map(renderType).mkString(" | ")}"
      case other ⇒ s"type alias ${decl.id} = ${renderType(other)}"
    }

  def renderType(tpe: Type): String =
    tpe match {
      case Ref(id)             => id
      case StrLiteral(str)     => "\"" + escape(str) + "\""
      case CharLiteral(ch)     => "'" + ch + "'"
      case NumLiteral(num)     => num.toString
      case FloatingLiteral(fl) => fl.toString
      case BoolLiteral(bool)   => bool.toString
      case Str                 => "String"
      case Character           => "Char"
      case Num                 => "Int"
      case Floating            => "Float"
      case Bool                => "Bool"
      case Optional(optTpe)    => "Maybe " + renderType(optTpe)
      case Array(arrTpe)       => "List " + renderType(arrTpe)
      case Struct(fields)      => fields.map(renderField).mkString("{ ", ", ", " }")
      case Union(types)        => types.map(renderType).mkString(" | ")
      case Intersection(types) =>
        // Elm doesn't support intersection types. To support compatibility with Typescript we just ignore the Struct part of the Intersection and treat it as union
        types.collect { case c: Ref ⇒ c }.map(renderType).mkString(" | ")
    }

  def renderField(field: (String, Type)): String =
    s"""${field._1}: ${renderType(field._2)}"""
}

object Renderer {
  implicit object TypescriptRenderer extends TypescriptStyleRenderer[Typescript]
  implicit object FlowRenderer extends TypescriptStyleRenderer[Flow]
  implicit object ElmRenderer extends ElmStyleRenderer[Elm]
}
