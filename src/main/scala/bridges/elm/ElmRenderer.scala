package bridges.elm

import bridges.core._
import bridges.core.Type._
import org.apache.commons.lang3.StringEscapeUtils.{escapeJava => escape}

trait ElmRenderer {
  implicit val renderer: Renderer[Elm] =
    new Renderer[Elm] {
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
          case UUIDLiteral(uuid)   => uuid.toString
          case Str                 => "String"
          case Character           => "Char"
          case Num                 => "Int"
          case Floating            => "Float"
          case Bool                => "Bool"
          case UUIDType            => "Uuid"
          case Optional(optTpe)    => "(Maybe " + renderType(optTpe) + ")"
          case Array(arrTpe)       => "(List " + renderType(arrTpe) + ")"
          case Struct(fields)      => fields.map(renderField).mkString("{ ", ", ", " }")
          case Union(types)        => types.map(renderType).mkString(" | ")
          case Intersection(_, iTpe, fields) =>
            val mainType = renderType(iTpe)
            val params = fields.fields.map { case (_, vTpe) ⇒ renderType(vTpe) }.mkString(" ")

            // we trim in case we have no params (case object) as tests don't like extra spaces
            s"$mainType $params".trim
        }

      def renderField(field: (String, Type)): String =
        s"""${field._1}: ${renderType(field._2)}"""
    }
}
