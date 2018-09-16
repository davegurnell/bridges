package bridges.elm

import bridges.core._
import bridges.core.Type._
import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }

trait ElmRenderer {

  def render(decl: Declaration): String = render(decl, Map.empty)

  def render(decl: Declaration, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    decl.tpe match {
//      case Union(types) ⇒
//        s"type ${decl.id} = ${types.map(renderType(_, customTypeReplacements)).mkString(" | ")}"
      case other ⇒ s"type alias ${decl.id} = ${renderType(other, customTypeReplacements)}"
    }

  private def renderType(tpe: Type, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    tpe match {
      case r @ Ref(id)      => customTypeReplacements.get(r).map(_.newType).getOrElse(id)
      case Str              => "String"
      case Character        => "Char"
      case Num              => "Int"
      case Floating         => "Float"
      case Bool             => "Bool"
      case Optional(optTpe) => "(Maybe " + renderType(optTpe, customTypeReplacements) + ")"
      case Array(arrTpe)    => "(List " + renderType(arrTpe, customTypeReplacements) + ")"
//      case AProduct(fields)    => fields.map(renderField(_, customTypeReplacements)).mkString("{ ", ", ", " }")
//      case Union(types)        => types.map(renderType(_, customTypeReplacements)).mkString(" | ")
//      case Intersection(_, iTpe, fields) =>
//        val mainType = renderType(iTpe, customTypeReplacements)
//        val params   = fields.fields.map { case (_, vTpe) ⇒ renderType(vTpe, customTypeReplacements) }.mkString(" ")
//
//         we trim in case we have no params (case object) as tests don't like extra spaces
//        s"$mainType $params".trim
      //TODO: fix and remove default case
      case _ ⇒ ""
    }

  private def renderField(field: (String, Type), customTypeReplacements: Map[Ref, TypeReplacement]): String =
    s"""${field._1}: ${renderType(field._2, customTypeReplacements)}"""
}
