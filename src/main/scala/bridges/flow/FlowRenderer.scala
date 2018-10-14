package bridges.flow

import bridges.core._
import bridges.core.Type._

trait FlowRenderer {

  def render(decl: Decl): String =
    render(decl, Map.empty)

  // 'intersectionMappings' is used to override intersection types,
  // as in some cases we want to not use the default product type but a custom value.
  def render(decl: Decl, intersectionMappings: Map[Type, String]): String =
    s"export type ${decl.name} = ${renderType(decl.tpe, intersectionMappings)};"

  private def precedence(tpe: Type): Int =
    tpe match {
      case _: Ref  => 1000
      case _ @Str  => 1000
      case _ @Chr  => 1000
      case _ @Intr => 1000
      case _ @Real => 1000
      case _ @Bool => 1000
      case _: Arr  => 800
      case _: Opt  => 600
      case _: Prod => 400
      case _: Sum  => 200
    }

  private def renderType(tpe: Type, intersectionMappings: Map[Type, String]): String =
    tpe match {
      case Ref(id)      => id
      case Str          => "string"
      case Chr          => "string"
      case Intr         => "number"
      case Real         => "number"
      case Bool         => "boolean"
      case Opt(optTpe)  => "?" + renderParens(tpe, intersectionMappings)(optTpe)
      case Arr(arrTpe)  => renderParens(tpe, intersectionMappings)(arrTpe) + "[]"
      case Prod(fields) => fields.map(renderField(_, intersectionMappings)).mkString("{ ", ", ", " }")
      case Sum(types) =>
        types
          .map {
            case DeclF(name, prod) ⇒
              val intersectionType = intersectionMappings.getOrElse(prod, s"""{ type: "$name" }""")
              s"""($intersectionType & $name)"""
          }
          .mkString(" | ")
    }

  private def renderField(field: Decl, intersectionMappings: Map[Type, String]): String =
    s"""${field.name}: ${renderType(field.tpe, intersectionMappings)}"""

  private def renderParens(outer: Type, intersectionMappings: Map[Type, String])(inner: Type): String =
    if (precedence(outer) >= precedence(inner)) {
      s"(${renderType(inner, intersectionMappings)})"
    } else {
      renderType(inner, intersectionMappings)
    }
}
