package bridges.ts

import org.apache.commons.lang3.StringEscapeUtils
import unindent._

object Renderer {
  import Type._

  def render(bindings: List[Binding]): String =
    bindings.map(renderTopLevel).mkString("\n\n")

  def renderTopLevel(binding: Binding): String =
    binding match {
      case Binding(id, union: Union) => s"export type $id =\n  ${renderType(true)(union)}"
      case Binding(id, tpe)          => s"export type $id = ${renderType(true)(tpe)}"
    }

  def renderType(topLevel: Boolean)(tpe: Type): String =
    tpe match {
      case Binding(id, tpe)  => id
      case Ref(id)           => id
      case StrLiteral(str)   => "\"" + StringEscapeUtils.escapeJava(str) + "\""
      case NumLiteral(num)   => num.toString
      case BoolLiteral(bool) => bool.toString
      case Str               => "string"
      case Num               => "number"
      case Bool              => "boolean"
      case Null              => "null"
      case Array(tpe)        => "Array<" + renderType(false)(tpe) + ">"
      case Union(types)      =>
        if(topLevel) {
          types.map(renderType(false)).mkString(" |\n  ")
        } else {
          types.map(renderType(false)).mkString(" | ")
        }
      case Struct(fields)    =>
        if(topLevel) {
          fields.map(renderField).mkString("{\n  ", ",\n  ", "\n}")
        } else {
          fields.map(renderField).mkString("{ ", ",  ", " }")
        }
    }

  def renderField(binding: Binding): String =
    s"${binding.id}: ${renderType(false)(binding.`type`)}"
}
