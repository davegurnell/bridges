package bridges.typescript

import bridges.core.Renderer
import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }

trait TsRenderer extends Renderer[TsType] {
  import TsType._

  def render(decl: TsDecl): String =
    s"export type ${decl.name} = ${renderType(decl.tpe)};"

  private def renderType(tpe: TsType): String =
    tpe match {
      case Ref(id)            => id
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

  private def renderStruct(fields: List[TsDecl]): String =
    fields.map(renderField).mkString("{ ", ", ", " }")

  private def renderField(field: TsDecl): String =
    s"""${field.name}: ${renderType(field.tpe)}"""

  private def renderParens(outer: TsType)(inner: TsType): String =
    if (precedence(outer) >= precedence(inner)) {
      s"(${renderType(inner)})"
    } else {
      renderType(inner)
    }

  private def precedence(tpe: TsType): Int =
    tpe match {
      case _: Ref     => 1000
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
