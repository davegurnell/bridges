package bridges.elm

import bridges.core._
import bridges.core.Type._
import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }

trait ElmRenderer {

  def render(decl: Declaration): String = render(decl, Map.empty)

  def render(decl: Declaration, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    decl.tpe match {
      case SumOfProducts(products) ⇒
        s"type ${decl.id} = ${products.map(renderSumType(_, customTypeReplacements)).mkString(" | ")}"
      case other ⇒ s"type alias ${decl.id} = ${renderType(other, customTypeReplacements)}"
    }

  private def renderSumType(aProduct: AProduct, customTypeReplacements: Map[Ref, TypeReplacement]) = {
    val refName  = Ref(aProduct.name)
    val mainType = customTypeReplacements.get(refName).map(_.newType).getOrElse(aProduct.name)
    val params   = aProduct.struct.fields.map { case (_, vTpe) ⇒ renderType(vTpe, customTypeReplacements) }.mkString(" ")
    //         we trim in case we have no params (case object) as tests don't like extra spaces
    s"$mainType $params".trim
  }

  private def renderType(tpe: Type, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    tpe match {
      case r @ Ref(id)         => customTypeReplacements.get(r).map(_.newType).getOrElse(id)
      case Str                 => "String"
      case Character           => "Char"
      case Num                 => "Int"
      case Floating            => "Float"
      case Bool                => "Bool"
      case Optional(optTpe)    => "(Maybe " + renderType(optTpe, customTypeReplacements) + ")"
      case Array(arrTpe)       => "(List " + renderType(arrTpe, customTypeReplacements) + ")"
      case Struct(fields)      => fields.map(renderField(_, customTypeReplacements)).mkString("{ ", ", ", " }")
      case AProduct(_, struct) => renderType(struct, customTypeReplacements)
      case _: SumOfProducts    => throw new IllegalArgumentException("SumOfProducts Renderer: we should never be here")
    }

  private def renderField(field: (String, Type), customTypeReplacements: Map[Ref, TypeReplacement]): String =
    s"""${field._1}: ${renderType(field._2, customTypeReplacements)}"""
}
