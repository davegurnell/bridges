package bridges.typescript

import bridges.core._
import bridges.core.Type._

trait TypescriptRenderer {
  def render(decl: Decl): String =
    render(decl, Map.empty)

  // 'intersectionMappings' is used to override intersection types, as in some cases we want to not use
  // the default AProduct type but a custom value.
  def render(decl: Decl, intersectionMappings: Map[Type, String]): String =
    s"export type ${decl.name} = ${renderType(decl.tpe, intersectionMappings)};"

  private def renderType(tpe: Type, intersectionMappings: Map[Type, String]): String =
    tpe match {
      case Ref(id)      => id
      case Str          => "string"
      case Chr          => "string"
      case Intr         => "number"
      case Real         => "number"
      case Bool         => "boolean"
      case Opt(optTpe)  => "(" + renderType(optTpe, intersectionMappings) + " | null)"
      case Arr(arrTpe)  => renderType(arrTpe, intersectionMappings) + "[]"
      case Prod(fields) => fields.map(renderField(_, intersectionMappings)).mkString("{ ", ", ", " }")
      case Sum(products) =>
        products
          .map {
            case DeclF(name, prod) ⇒
              val intersectionType = intersectionMappings.getOrElse(prod, s"""{ type: "$name" }""")
              s"""($intersectionType & $name)"""
          }
          .mkString("(", " | ", ")")
    }

  private def renderField(field: Decl, intersectionMappings: Map[Type, String]): String =
    s"""${field.name}: ${renderType(field.tpe, intersectionMappings)}"""
}
