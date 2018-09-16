package bridges.flow

import bridges.core._
import bridges.core.Type._

trait FlowRenderer {

  def render(decl: Declaration): String =
    render(decl, Map.empty)

  // 'intersectionMappings' is used to override intersection types, as in some cases we want to not use
  // the default AProduct type but a custom value.
  def render(decl: Declaration, intersectionMappings: Map[Type, String]): String =
    s"export type ${decl.id} = ${renderType(decl.tpe, intersectionMappings)};"

  private def precedence(tpe: Type): Int =
    tpe match {
      case _: Ref           => 1000
      case _ @Str           => 1000
      case _ @Character     => 1000
      case _ @Num           => 1000
      case _ @Floating      => 1000
      case _ @Bool          => 1000
      case _: Array         => 900
      case _: Optional      => 800
      case _: AProduct      => 600
      case _: SumOfProducts => 400
      case _                ⇒ 0
    }

  private def renderType(tpe: Type, intersectionMappings: Map[Type, String]): String =
    tpe match {
      case Ref(id)             => id
      case Str                 => "string"
      case Character           => "string"
      case Num                 => "number"
      case Floating            => "number"
      case Bool                => "boolean"
      case Optional(optTpe)    => "?" + renderParens(tpe, intersectionMappings)(optTpe)
      case Array(arrTpe)       => renderParens(tpe, intersectionMappings)(arrTpe) + "[]"
      case Struct(fields)      => fields.map(renderField(_, intersectionMappings)).mkString("{ ", ", ", " }")
      case AProduct(_, struct) => renderType(struct, intersectionMappings)
      case SumOfProducts(products) =>
        products
          .map { prd ⇒
            val intersectionType = intersectionMappings.getOrElse(prd, s"""{ type: "${prd.name}" }""")
            s"""($intersectionType & ${prd.name})"""

          }
          .mkString(" | ")
    }

  private def renderField(field: (String, Type), intersectionMappings: Map[Type, String]): String = {
    val (name, tpe) = field
    s"""$name: ${renderType(tpe, intersectionMappings)}"""
  }

  private def renderParens(outer: Type, intersectionMappings: Map[Type, String])(inner: Type): String =
    if (precedence(outer) >= precedence(inner)) {
      s"(${renderType(inner, intersectionMappings)})"
    } else {
      renderType(inner, intersectionMappings)
    }
}
