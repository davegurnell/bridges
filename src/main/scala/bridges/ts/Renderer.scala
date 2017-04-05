package bridges.ts

import unindent._

object Renderer {
  import TsType._

  def render(bindings: List[Binding]): String =
    bindings.map(renderTopLevel).mkString("\n\n")

  def renderTopLevel(binding: Binding): String =
    binding match {
      case Binding(id, union: Union) => s"type $id =\n  ${renderType(union)}"
      case Binding(id, tpe)          => s"type $id = ${renderType(tpe)}"
    }

  def renderType(tpe: TsType): String =
    tpe match {
      case Str              => "string"
      case Num              => "number"
      case Bool             => "boolean"
      case Null             => "null"
      case Ref(id)          => id
      case Binding(id, tpe) => id
      case Union(types)     => types.map(renderType).mkString(" |\n  ")
      case Struct(fields)   => fields.map(renderField).mkString("{\n  ", ",\n  ", "\n}")
    }

  def renderField(binding: Binding): String =
    s"${binding.id}: ${renderType(binding.`type`)}"
}
