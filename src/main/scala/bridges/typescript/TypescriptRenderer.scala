package bridges.typescript

import bridges.core._
import bridges.core.Type._
import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }

trait TypescriptRenderer {

  def render(decl: Declaration): String =
    render(decl, Map.empty)

  // 'intersectionMappings' is used to override intersection types, as in some cases we want to not use
  // the default AProduct type but a custom value.
  def render(decl: Declaration, intersectionMappings: Map[Type, String]): String =
    s"export type ${decl.id} = ${renderType(decl.tpe, intersectionMappings)};"

  private def renderType(tpe: Type, intersectionMappings: Map[Type, String]): String =
    tpe match {
      case Ref(id)             => id
      case Str                 => "string"
      case Character           => "string"
      case Num                 => "number"
      case Floating            => "number"
      case Bool                => "boolean"
      case Optional(optTpe)    => "(" + renderType(optTpe, intersectionMappings) + " | null)"
      case Array(arrTpe)       => renderType(arrTpe, intersectionMappings) + "[]"
      case Struct(fields)      => fields.map(renderField(_, intersectionMappings)).mkString("{ ", ", ", " }")
      case AProduct(_, struct) => renderType(struct, intersectionMappings)
      case SumOfProducts(products) =>
        products
          .map { prd ⇒
            val intersectionType = intersectionMappings.getOrElse(prd, s"""{ type: "${prd.name}" }""")
            s"""($intersectionType & ${prd.name})"""

          }
          .mkString("(", " | ", ")")
    }

  private def renderField(field: (String, Type), intersectionMappings: Map[Type, String]): String =
    s"""${field._1}: ${renderType(field._2, intersectionMappings)}"""
}
