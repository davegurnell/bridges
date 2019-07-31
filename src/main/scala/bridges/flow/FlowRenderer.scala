package bridges.flow

import bridges.core.Renderer
import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }

trait FlowRenderer extends Renderer[FlowType] {
  import FlowType._

  def render(decl: FlowDecl): String =
    s"""export type ${renderParams(decl.name, decl.params)} = ${renderType(decl.tpe)};"""

  private def renderParams(name: String, params: List[String]): String =
    if (params.isEmpty) name else params.mkString(s"$name<", ", ", ">")

  private def renderType(tpe: FlowType): String =
    tpe match {
      case Ref(id, params)    => renderRef(id, params)
      case Str                => "string"
      case Chr                => "string"
      case Intr               => "number"
      case Real               => "number"
      case Bool               => "boolean"
      case Null               => "null"
      case Undefined          => "undefined"
      case StrLit(value)      => s""""${escape(value)}""""
      case ChrLit(value)      => s""""${escape(value.toString)}""""
      case IntrLit(value)     => value.toString
      case RealLit(value)     => value.toString
      case BoolLit(value)     => value.toString
      case tpe @ Opt(arg)     => s"""?${renderParens(tpe)(arg)}"""
      case tpe @ Arr(arg)     => s"""${renderParens(tpe)(arg)}[]"""
      case Tuple(types)       => types.map(renderType).mkString("[", ", ", "]")
      case Struct(fields)     => renderStruct(fields)
      case tpe @ Inter(types) => types.map(renderParens(tpe)).mkString(" & ")
      case tpe @ Union(types) => types.map(renderParens(tpe)).mkString(" | ")
    }

  private def renderRef(name: String, params: List[FlowType]): String =
    if (params.isEmpty) name else params.map(renderType).mkString(s"$name<", ", ", ">")

  private def renderStruct(fields: List[(String, FlowType)]): String =
    fields.map(renderField).mkString("{ ", ", ", " }")

  private def renderField(field: (String, FlowType)): String =
    s"""${field._1}: ${renderType(field._2)}"""

  private def renderParens(outer: FlowType)(inner: FlowType): String =
    if (precedence(outer) > precedence(inner)) {
      s"(${renderType(inner)})"
    } else {
      renderType(inner)
    }

  private def precedence(tpe: FlowType): Int =
    tpe match {
      case _: Ref       => 1000
      case _ @Str       => 1000
      case _ @Chr       => 1000
      case _ @Intr      => 1000
      case _ @Real      => 1000
      case _ @Bool      => 1000
      case _ @Null      => 1000
      case _ @Undefined => 1000
      case _: StrLit    => 1000
      case _: ChrLit    => 1000
      case _: IntrLit   => 1000
      case _: RealLit   => 1000
      case _: BoolLit   => 1000
      case _: Arr       => 900
      case _: Tuple     => 900
      case _: Opt       => 800
      case _: Struct    => 600
      case _: Union     => 400
      case _: Inter     => 200
    }
}
