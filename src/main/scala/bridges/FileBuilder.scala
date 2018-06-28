package bridges

import bridges.Type._
import unindent._
import bridges.syntax._

trait FileBuilder[A] {
  // Given a declaration, returns a Map where 'keys' are file names and 'values' are the contents
  def buildFile(module: String, decl: Declaration): Map[String, String]
  def buildFile(module: String, decls: List[Declaration]): Map[String, String] =
    decls.flatMap(buildFile(module, _)).toMap
}

trait ElmFileBuilder extends FileBuilder[Elm] {

  def buildFile(module: String, decl: Declaration): Map[String, String] = {
    val tpeName = decl.id

    val referencedTypes = getDeclarationTypes(decl.tpe, tpeName)
      .map(r ⇒ s"import $module.${r.id} as ${r.id} exposing (${r.id})")
      .mkString("\n")

    val content =
      i"""
       module $module.$tpeName exposing ($tpeName, decoder, encoder)

       import Json.Decode as Decode
       import Json.Decode.Pipeline exposing (..)
       import Json.Encode as Encode
       import Uuid exposing (Uuid)
       $referencedTypes

       ${render[Elm](decl)}

       ${jsonDecoder[Elm](decl)}

       ${jsonEncoder[Elm](decl)}
       """

    Map(s"$tpeName.elm" → content)
  }

  def getDeclarationTypes(tpe: Type, parentType: String): List[Ref] = {
    def getExcludeTypes(tpe: Type): List[Ref] = {
      tpe match {
        case r @ Ref(_)               => r :: Nil
        case Optional(optTpe)         => getExcludeTypes(optTpe)
        case Array(arrTpe)            => getExcludeTypes(arrTpe)
        case Struct(fields)           => fields.map(_._2).flatMap(getExcludeTypes)
        case Union(uTpe)              => uTpe.flatMap(getExcludeTypes)
        case Intersection(_, iTpe, _) => getExcludeTypes(iTpe)
        case _ ⇒ Nil
      }
    }

    def getIncludeTypes(tpe: Type): List[Ref] = {
      tpe match {
        case r @ Ref(_)       => r :: Nil
        case Optional(optTpe) => getIncludeTypes(optTpe)
        case Array(arrTpe)    => getIncludeTypes(arrTpe)
        case Struct(fields)   => fields.map(_._2).flatMap(getIncludeTypes)
        case Union(uTpe)      => uTpe.flatMap(getIncludeTypes)
        case Intersection(_, _, fields) =>
          fields.fields.map(_._2).flatMap(getIncludeTypes)
        case _ ⇒ Nil
      }
    }

    val exclude = Ref(parentType) :: getExcludeTypes(tpe)
    val include = getIncludeTypes(tpe)

    include.filterNot(exclude.contains).distinct
  }
}

object FileBuilder {
  implicit object ElmFileBuilder extends ElmFileBuilder
}
