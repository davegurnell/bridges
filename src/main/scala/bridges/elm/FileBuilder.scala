package bridges.elm

import bridges.core._
import bridges.core.Type._
import bridges.syntax._
import unindent._

trait ElmFileBuilder {
  implicit val fileBuilder: FileBuilder[Elm] =
    new FileBuilder[Elm] {
      def buildFile(module: String, decls: List[Declaration]): (String, String) = {
        val fileName = decls.headOption.map(_.id).getOrElse("")
        val foldZero = ("", "", "")

        val typesInFile = decls.map(_.id)
        val imports = decls
          .flatMap(d ⇒ getDeclarationTypes(d.tpe, d.id))
          .distinct
          .filterNot(r ⇒ typesInFile.contains(r.id))
          .map(r ⇒ s"import $module.${r.id} exposing (..)")
          .mkString("\n")

        val (declarations, decoders, encoders) =
          decls.map(getFileComponents(module, _)).foldLeft(foldZero) {
            case (acc, (t, d, e)) ⇒
              (
                s"${acc._1}\n$t",
                s"${acc._2}\n\n$d",
                s"${acc._3}\n\n$e"
              )
          }

        val pipelineImport =
          if (decoders.contains("required"))
            "import Json.Decode.Pipeline exposing (..)"
          else ""

        val uuidImport =
          if (decoders.contains("Uuid") || encoders.contains("Uuid"))
            "import Uuid exposing (Uuid)"
          else ""

        val content =
          i"""
           module $module.$fileName exposing (..)

           import Json.Decode as Decode
           $pipelineImport
           import Json.Encode as Encode
           $uuidImport
           $imports

           $declarations

           $decoders

           $encoders
           """

        (s"$fileName.elm", content)
      }

      private def getFileComponents(
          module: String,
          decl: Declaration
      ): (String, String, String) = {
        val declaration = render[Elm](decl)
        val decoder     = jsonDecoder[Elm](decl)
        val encoder     = jsonEncoder[Elm](decl)

        (declaration, decoder, encoder)
      }

      def getDeclarationTypes(tpe: Type, parentType: String): List[Ref] = {
        def getIncludeTypes(tpe: Type): List[Ref] =
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

        val exclude = Ref(parentType)
        val include = getIncludeTypes(tpe)

        include.distinct.filterNot(_ == exclude)
      }
    }
}
