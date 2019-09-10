package bridges.elm

import bridges.core.Type._
import bridges.core._
import unindent._

trait ElmFileBuilder {
  // Given a declaration, returns a tuple with file name and file contents:
  def buildFile(module: String, decl: Decl, customTypeReplacements: Map[Ref, TypeReplacement] = Map.empty): (String, String) =
    buildFile(module, List(decl), customTypeReplacements)

  def buildFile(module: String, decls: List[Decl], customTypeReplacements: Map[Ref, TypeReplacement]): (String, String) = {
    val fileName = decls.headOption.map(_.name).getOrElse("")
    val foldZero = ("", "", "")

    val typesInFile      = decls.map(_.name)
    val replacementTypes = customTypeReplacements.keySet
    val typeImports = decls
      .flatMap(d => getDeclarationTypes(d.tpe, d.name))
      .distinct
      .filterNot(r => typesInFile.contains(r.id) || replacementTypes.contains(r))
      .map(r => s"import $module.${r.id} exposing (..)")
      .mkString("\n")

    val (declarations, decoders, encoders) =
      decls.map(getFileComponents(module, customTypeReplacements, _)).foldLeft(foldZero) {
        case (acc, (t, d, e)) =>
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

    val customImports = customTypeReplacements.values.filter(td => decoders.contains(td.newType)).flatMap(_.imports).mkString("\n")

    val imports = typeImports + customImports

    val content =
      i"""
           module $module.$fileName exposing (..)

           import Json.Decode as Decode
           $pipelineImport
           import Json.Encode as Encode
           $imports
           $declarations
           $decoders
           $encoders
           """

    (s"$fileName.elm", content)
  }

  private def getFileComponents(
      module: String,
      customTypeReplacements: Map[Ref, TypeReplacement],
      decl: Decl
  ): (String, String, String) = {
    val declaration = Elm.render(decl, customTypeReplacements)
    val decoder     = Elm.decoder(decl, customTypeReplacements)
    val encoder     = Elm.encoder(decl, customTypeReplacements)

    (declaration, decoder, encoder)
  }

  private def getDeclarationTypes(tpe: Type, parentType: String): List[Ref] = {
    def getIncludeTypes(tpe: Type): List[Ref] =
      tpe match {
        case r: Ref           => r :: Nil
        case Opt(optTpe)      => getIncludeTypes(optTpe)
        case Arr(arrTpe)      => getIncludeTypes(arrTpe)
        case Dict(kTpe, vTpe) => Ref("Dict") +: (getIncludeTypes(kTpe) ++ getIncludeTypes(vTpe))
        case Prod(fields)     => fields.map { case (_, tpe) => tpe }.flatMap(getIncludeTypes)
        case Sum(products)    => products.map { case (_, tpe) => tpe }.flatMap(getIncludeTypes)
        case _                => Nil
      }

    val exclude = Ref(parentType)
    val include = getIncludeTypes(tpe)

    include.distinct.filterNot(_ == exclude)
  }
}
