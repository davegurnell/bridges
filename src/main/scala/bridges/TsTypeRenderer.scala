package bridges

import org.apache.commons.text.StringEscapeUtils.{escapeJava => escape}

abstract class TsTypeRenderer(exportAll: Boolean) {
  def render(decls: List[TsDecl]): String =
    decls.map(render).mkString("\n\n")

  def render(decl: TsDecl): String =
    decl match {
      case TsDecl(name, params, TsType.Struct(fields, rest)) =>
        s"${if (exportAll) "export interface" else "interface"} ${renderParams(name, params)} ${renderStructAsInterface(fields, rest)}"

      case TsDecl(name, params, tpe) =>
        s"${if (exportAll) "export type" else "type"} ${renderParams(name, params)} = ${renderType(tpe)};"
    }

  def renderType(tpe: TsType): String =
    tpe match {
      case TsType.Ref(id, params)      => renderRef(id, params)
      case TsType.Any                  => "any"
      case TsType.Str                  => "string"
      case TsType.Num                  => "number"
      case TsType.Bool                 => "boolean"
      case TsType.Null                 => "null"
      case TsType.Unknown              => "unknown"
      case TsType.StrLit(value)        => s""""${escape(value)}""""
      case TsType.NumLit(value)        => value.toString
      case TsType.BoolLit(value)       => value.toString
      case tpe @ TsType.Arr(arg)       => s"""${renderParens(tpe)(arg)}[]"""
      case TsType.Tuple(types)         => types.map(renderType).mkString("[", ", ", "]")
      case TsType.Func(args, ret)      => s"""${renderArgs(args)} => ${renderType(ret)}"""
      case TsType.Struct(fields, rest) => renderStruct(fields, rest)
      case TsType.Record(key, value)   => renderRecord(key, value)
      case tpe @ TsType.Inter(types)   => types.map(renderParens(tpe)).mkString(" & ")
      case tpe @ TsType.Union(types)   => types.map(renderParens(tpe)).mkString(" | ")
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

  private def renderRecord(key: TsType, value: TsType): String =
    s"Record<${renderType(key)}, ${renderType(value)}>"

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
      case _: TsType.Ref     => 1000
      case _ @TsType.Any     => 1000
      case _ @TsType.Unknown => 1000
      case _ @TsType.Str     => 1000
      case _ @TsType.Num     => 1000
      case _ @TsType.Bool    => 1000
      case _ @TsType.Null    => 1000
      case _: TsType.StrLit  => 1000
      case _: TsType.NumLit  => 1000
      case _: TsType.BoolLit => 1000
      case _: TsType.Arr     => 900
      case _: TsType.Tuple   => 900
      case _: TsType.Struct  => 600
      case _: TsType.Record  => 900
      case _: TsType.Union   => 400
      case _: TsType.Inter   => 200
      case _: TsType.Func    => 100
    }
}
