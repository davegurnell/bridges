package bridges.typescript

import bridges.core.{ DeclF, Renderer }
import org.apache.commons.text.StringEscapeUtils.{ escapeJava => escape }

abstract class TsTypeRenderer(exportAll: Boolean) extends Renderer[TsType] {
  import TsType._

  def render(decl: TsDecl): String =
    decl match {
      case DeclF(name, params, TsType.Struct(fields, rest)) =>
        s"${if (exportAll) "export interface" else "interface"} ${renderParams(name, params)} ${renderStructAsInterface(fields, rest)}"

      case DeclF(name, params, tpe) =>
        s"${if (exportAll) "export type" else "type"} ${renderParams(name, params)} = ${renderType(tpe)};"
    }

  def renderType(tpe: TsType): String =
    tpe match {
      case Ref(id, params)      => renderRef(id, params)
      case Any                  => "any"
      case Str                  => "string"
      case Chr                  => "string"
      case Intr                 => "number"
      case Real                 => "number"
      case Bool                 => "boolean"
      case Null                 => "null"
      case Unknown              => "unknown"
      case StrLit(value)        => s""""${escape(value)}""""
      case ChrLit(value)        => s""""${escape(value.toString)}""""
      case IntrLit(value)       => value.toString
      case RealLit(value)       => value.toString
      case BoolLit(value)       => value.toString
      case tpe @ Arr(arg)       => s"""${renderParens(tpe)(arg)}[]"""
      case Tuple(types)         => types.map(renderType).mkString("[", ", ", "]")
      case Func(args, ret)      => s"""${renderArgs(args)} => ${renderType(ret)}"""
      case Struct(fields, rest) => renderStruct(fields, rest)
      case tpe @ Inter(types)   => types.map(renderParens(tpe)).mkString(" & ")
      case tpe @ Union(types)   => types.map(renderParens(tpe)).mkString(" | ")
    }

  private def renderParams(name: String, params: List[String]): String =
    if (params.isEmpty) name else params.mkString(s"$name<", ", ", ">")

  private def renderRef(name: String, params: List[TsType]): String =
    if (params.isEmpty) name else params.map(renderType).mkString(s"$name<", ", ", ">")

  private def renderStruct(fields: List[TsField], rest: Option[TsRestField]): String =
    (fields.map(renderField) ++ rest.toList.map(renderRestField))
      .mkString("{ ", ", ", " }")

  private def renderStructAsInterface(fields: List[TsField], rest: Option[TsRestField]): String =
    (fields.map(renderField) ++ rest.toList.map(renderRestField))
      .map(str => s"  $str;\n")
      .mkString("{\n", "", "}")

  private def renderField(field: TsField): String =
    field match {
      case TsField(name, valueType, false) =>
        s"""${name}: ${renderType(valueType)}"""

      case TsField(name, valueType, true) =>
        s"""${name}?: ${renderType(valueType)}"""
    }

  private def renderArgs(args: List[(String, TsType)]): String =
    args
      .map { case (name, tpe) => s"""${name}: ${renderType(tpe)}""" }
      .mkString("(", ", ", ")")

  private def renderRestField(field: TsRestField): String = {
    val TsRestField(name, keyType, valueType) = field
    s"""[${name}: ${renderType(keyType)}]: ${renderType(valueType)}"""
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
      case _ @Unknown => 1000
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
      case _: Tuple   => 900
      case _: Struct  => 600
      case _: Union   => 400
      case _: Inter   => 200
      case _: Func    => 100
    }
}
