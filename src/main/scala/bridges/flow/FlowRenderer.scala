package bridges.flow

import bridges.core._
import bridges.core.Type._
import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }

trait FlowRenderer {
  implicit val renderer: Renderer[Flow] =
    new Renderer[Flow] {
      def render(decl: Declaration): String =
        s"export type ${decl.id} = ${renderType(decl.tpe)};"

      def precedence(tpe: Type): Int =
        tpe match {
          case _: Ref             => 1000
          case _: StrLiteral      => 1000
          case _: CharLiteral     => 1000
          case _: NumLiteral      => 1000
          case _: FloatingLiteral => 1000
          case _: BoolLiteral     => 1000
          case _: UUIDLiteral     => 1000
          case _: Str             => 1000
          case _: Character       => 1000
          case _: Num             => 1000
          case _: Floating        => 1000
          case _: Bool            => 1000
          case _: UUIDType        => 1000
          case _: Optional        => 800
          case _: Array           => 800
          case _: Struct          => 600
          case _: Union           => 400
          case _: Intersection    => 200
        }

      def renderType(tpe: Type): String =
        tpe match {
          case Ref(id)                => id
          case StrLiteral(str)        => "\"" + escape(str) + "\""
          case CharLiteral(ch)        => "\"" + ch + "\""
          case NumLiteral(num)        => num.toString
          case FloatingLiteral(float) => float.toString
          case BoolLiteral(bool)      => bool.toString
          case UUIDLiteral(uuid)      => uuid.toString // Flow doesn't have native UUID type
          case Str                    => "string"
          case Character              => "string"
          case Num                    => "number"
          case Floating               => "number"
          case Bool                   => "boolean"
          case UUIDType               => "string" // Flow doesn't have native UUID type
          case Optional(optTpe)       => "?" + renderParens(tpe)(optTpe)
          case Array(arrTpe)          => renderParens(tpe)(arrTpe) + "[]"
          case Struct(fields)         => fields.map(renderField).mkString("{ ", ", ", " }")
          // case Union(types) if types contains Null => renderType(Optional(Union(types.filterNot(_ == Null))))
          case Union(types) => types.map(renderParens(tpe)).mkString(" | ")
          // Flow doesn't care about the fields when building Union types as they act as references to a type defined somewhere else
          case Intersection(key, iTpe, _) => List(key, iTpe).map(renderParens(tpe)).mkString(" & ")
        }

      def renderField(field: (String, Type)): String =
        s"""${field._1}: ${renderType(field._2)}"""

      def renderParens(outer: Type)(inner: Type): String =
        if (precedence(outer) >= precedence(inner)) {
          s"(${renderType(inner)})"
        } else {
          renderType(inner)
        }
    }
}
