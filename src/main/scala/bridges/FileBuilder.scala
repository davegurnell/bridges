package bridges

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
    val content =
      i"""
       module $module.$tpeName exposing ($tpeName, decoder, encoder)

       import Json.Decode as Decode
       import Json.Decode.Pipeline exposing (..)
       import Json.Encode as Encode

       ${render[Elm](decl)}

       ${jsonDecoder[Elm](decl)}

       ${jsonEncoder[Elm](decl)}
       """

    Map(s"$tpeName.elm" → content)
  }
}

object FileBuilder {
  implicit object ElmFileBuilder extends ElmFileBuilder
}
