package bridges.elm

import bridges.core._
import bridges.core.Type._

trait ElmRenderer extends Renderer[Type] {
  def render(decl: Decl): String =
    render(decl, Map.empty)

  def render(decl: Decl, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    decl.tpe match {
      case Sum(products) ⇒
        s"type ${decl.name} = ${products.map(renderSumType(_, customTypeReplacements)).mkString(" | ")}"
      case other ⇒ s"type alias ${decl.name} = ${renderType(other, customTypeReplacements)}"
    }

  private def renderSumType(prod: DeclF[Type.Prod], customTypeReplacements: Map[Ref, TypeReplacement]) = {
    val refName  = Ref(prod.name)
    val mainType = customTypeReplacements.get(refName).map(_.newType).getOrElse(prod.name)
    val params   = prod.tpe.fields.map(field => renderType(field.tpe, customTypeReplacements)).mkString(" ")
    // We trim in case we have no params (case object) as tests don't like extra spaces:
    s"$mainType $params".trim
  }

  private def renderType(tpe: Type, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    tpe match {
      case r @ Ref(id)  => customTypeReplacements.get(r).map(_.newType).getOrElse(id)
      case Str          => "String"
      case Chr          => "Char"
      case Intr         => "Int"
      case Real         => "Float"
      case Bool         => "Bool"
      case Opt(optTpe)  => "(Maybe " + renderType(optTpe, customTypeReplacements) + ")"
      case Arr(arrTpe)  => "(List " + renderType(arrTpe, customTypeReplacements) + ")"
      case Prod(fields) => fields.map(renderField(_, customTypeReplacements)).mkString("{ ", ", ", " }")
      case _: Sum       => throw new IllegalArgumentException("SumOfProducts Renderer: we should never be here")
    }

  private def renderField(field: Decl, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    s"""${field.name}: ${renderType(field.tpe, customTypeReplacements)}"""
}
