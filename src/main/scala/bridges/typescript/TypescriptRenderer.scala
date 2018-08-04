package bridges.typescript

import bridges.core._
import bridges.core.Type._
import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }

trait TypescriptRenderer {
  implicit val renderer: Renderer[Typescript] =
    new Renderer[Typescript] {
      def render(decl: Declaration): String =
        s"export type ${decl.id} = ${renderType(decl.tpe)};"

      def renderType(tpe: Type): String =
        tpe match {
          case Ref(id)                => id
          case StrLiteral(str)        => "\"" + escape(str) + "\""
          case CharLiteral(ch)        => "\"" + ch + "\""
          case NumLiteral(num)        => num.toString
          case FloatingLiteral(float) => float.toString
          case BoolLiteral(bool)      => bool.toString
          case UUIDLiteral(uuid)      => uuid.toString // Typescript doesn't have native UUID type
          case Str                    => "string"
          case Character              => "string"
          case Num                    => "number"
          case Floating               => "number"
          case Bool                   => "boolean"
          case UUIDType               => "string" // Typescript doesn't have native UUID type
          case Optional(optTpe)       => "(" + renderType(optTpe) + " | null)"
          case Array(arrTpe)          => renderType(arrTpe) + "[]"
          case Struct(fields)         => fields.map(renderField).mkString("{ ", ", ", " }")
          case Union(types)           => types.map(renderType).mkString("(", " | ", ")")
          // Typescript languages don't care about the fields when building Union types as they act as references to a type defined somewhere else
          case Intersection(key, iTpe, _) => List(key, iTpe).map(renderType).mkString("(", " & ", ")")
        }

      def renderField(field: (String, Type)): String =
        s"""${field._1}: ${renderType(field._2)}"""
    }
}
