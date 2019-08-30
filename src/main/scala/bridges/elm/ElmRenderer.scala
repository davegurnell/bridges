package bridges.elm

import bridges.core._
import bridges.core.Type._

trait ElmRenderer extends Renderer[Type] with ElmUtils {
  def render(decl: Decl): String =
    render(decl, Map.empty)

  def render(decl: Decl, customTypeReplacements: Map[Ref, TypeReplacement]): String = {
    val (newTypeReplacements, genericsDefinition) = mergeGenericsAndTypes(decl, customTypeReplacements)
    val genericsInType                            = genericsDefinition.foldLeft("")((acc, b) => s"$acc $b")
    decl.tpe match {
      case Sum(products) =>
        s"type ${decl.name}$genericsInType = ${products.map { case (name, prod) => renderSumType(name, prod, newTypeReplacements) }.mkString(" | ")}"
      case other => s"type alias ${decl.name}$genericsInType = ${renderType(other, newTypeReplacements)}"
    }
  }

  private def renderSumType(name: String, prod: Prod, customTypeReplacements: Map[Ref, TypeReplacement]) = {
    val refName  = Ref(name)
    val mainType = customTypeReplacements.get(refName).map(_.newType).getOrElse(name)
    val params   = prod.fields.map { case (name, tpe) => renderType(tpe, customTypeReplacements) }.mkString(" ")
    // We trim in case we have no params (case object) as tests don't like extra spaces:
    s"$mainType $params".trim
  }

  private def renderType(tpe: Type, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    tpe match {
      case r @ Ref(id, _) => customTypeReplacements.get(r).map(_.newType).getOrElse(id)
      case Str            => "String"
      case Chr            => "Char"
      case Intr           => "Int"
      case Real           => "Float"
      case Bool           => "Bool"
      case Opt(optTpe)    => "(Maybe " + renderType(optTpe, customTypeReplacements) + ")"
      case Arr(arrTpe)    => "(List " + renderType(arrTpe, customTypeReplacements) + ")"
      case Prod(fields)   => fields.map { case (name, tpe) => renderField(name, tpe, customTypeReplacements) }.mkString("{ ", ", ", " }")
      case _: Sum         => throw new IllegalArgumentException("SumOfProducts Renderer: we should never be here")
    }

  private def renderField(name: String, tpe: Type, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    s"""$name: ${renderType(tpe, customTypeReplacements)}"""
}
