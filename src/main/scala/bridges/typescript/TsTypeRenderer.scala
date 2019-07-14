package bridges.typescript

import bridges.core.{ DeclF, Renderer }
import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }

abstract class TsTypeRenderer(exportAll: Boolean) extends Renderer[TsType] {
  import TsType._

  def render(decl: TsDecl): String =
    decl match {
      case DeclF(name, params, TsType.Struct(fields)) =>
        s"${if (exportAll) "export interface" else "interface"} ${renderParams(name, params)} ${renderStructAsInterface(fields)}"

      case DeclF(name, params, tpe) =>
        s"${if (exportAll) "export type" else "type"} ${renderParams(name, params)} = ${renderType(tpe)};"
    }

  private def renderType(tpe: TsType): String =
    tpe match {
      case Ref(id, params)    => renderRef(id, params)
      case Any                => "any"
      case Str                => "string"
      case Chr                => "string"
      case Intr               => "number"
      case Real               => "number"
      case Bool               => "boolean"
      case Null               => "null"
      case StrLit(value)      => s""""${escape(value)}""""
      case ChrLit(value)      => s""""${escape(value.toString)}""""
      case IntrLit(value)     => value.toString
      case RealLit(value)     => value.toString
      case BoolLit(value)     => value.toString
      case tpe @ Arr(arg)     => s"""${renderParens(tpe)(arg)}[]"""
      case Struct(fields)     => renderStruct(fields)
      case tpe @ Inter(types) => types.map(renderParens(tpe)).mkString(" & ")
      case tpe @ Union(types) => types.map(renderParens(tpe)).mkString(" | ")
    }

  private def renderParams(name: String, params: List[String]): String =
    if (params.isEmpty) name else params.mkString(s"$name<", ", ", ">")

  private def renderRef(name: String, params: List[TsType]): String =
    if (params.isEmpty) name else params.map(renderType).mkString(s"$name<", ", ", ">")

  private def renderStruct(fields: List[TsType.Field]): String =
    fields.map(renderField).mkString("{ ", ", ", " }")

  private def renderStructAsInterface(fields: List[TsType.Field]): String =
    fields.map(renderField).mkString("{\n  ", ";\n  ", ";\n}")

  private def renderField(field: TsType.Field): String = {
    val optFlag = if (field.optional) "?" else ""
    s"""${field.name}${optFlag}: ${renderType(field.tpe)}"""
  }

  private def renderParens(outer: TsType)(inner: TsType): String =
    if (precedence(outer) > precedence(inner)) {
      s"(${renderType(inner)})"
    } else {
      renderType(inner)
    }

  private def precedence(tpe: TsType): Int =
    tpe match {
      case _: Ref     => 1000
      case _ @Any     => 1000
      case _ @Str     => 1000
      case _ @Chr     => 1000
      case _ @Intr    => 1000
      case _ @Real    => 1000
      case _ @Bool    => 1000
      case _ @Null    => 1000
      case _: StrLit  => 1000
      case _: ChrLit  => 1000
      case _: IntrLit => 1000
      case _: RealLit => 1000
      case _: BoolLit => 1000
      case _: Arr     => 900
      case _: Struct  => 600
      case _: Union   => 400
      case _: Inter   => 200
    }
}
