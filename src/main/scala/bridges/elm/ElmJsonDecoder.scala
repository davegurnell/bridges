package bridges.elm

import bridges.core._
import bridges.core.Type._
import unindent._

trait ElmJsonDecoder extends ElmUtils {
  def decoder(decls: List[Decl], customTypeReplacements: Map[Ref, TypeReplacement]): String =
    decls.map(decoder(_, customTypeReplacements)).mkString("\n\n")

  def decoder(decl: Decl, customTypeReplacements: Map[Ref, TypeReplacement] = Map.empty): String = {
    val (newTypeReplacements, genericsDefinition) = mergeGenericsAndTypes(decl, customTypeReplacements)
    val genericsInType                            = genericsDefinition.foldLeft("")((acc, b) => s"$acc $b")
    val nameWithGenerics                          = if (genericsInType.isEmpty) decl.name else s"(${decl.name}$genericsInType)"
    val definitionsForGenerics                    = genericsDefinition.map(s => s"(Decode.Decoder $s) -> ").foldLeft("")((acc, b) => s"$acc$b")
    val methodsForGenerics                        = genericsDefinition.map(s => s"decoder${s.toUpperCase}").foldLeft("")((acc, b) => s"$acc $b")

    decl.tpe match {
      case Sum(products) =>
        // DO NOT REMOVE SPACE AT END - needed for Elm compiler and to pass tests. Yup, dirty, I know!
        val body = products.map { case (name, prod) => decodeSumType(name, prod, newTypeReplacements) }.mkString("\n      ")
        i"""
            decoder${decl.name} : ${definitionsForGenerics}Decode.Decoder $nameWithGenerics
            decoder${decl.name}$methodsForGenerics = Decode.field "type" Decode.string |> Decode.andThen decoder${decl.name}Tpe$methodsForGenerics

            decoder${decl.name}Tpe : ${definitionsForGenerics}String -> Decode.Decoder $nameWithGenerics
            decoder${decl.name}Tpe$methodsForGenerics tpe =
               case tpe of
                  $body
                  _ -> Decode.fail ("Unexpected type for ${decl.name}: " ++ tpe)
            """
      case other =>
        val body = decodeType(other, newTypeReplacements)

        i"""
            decoder${decl.name} : ${definitionsForGenerics}Decode.Decoder $nameWithGenerics
            decoder${decl.name}$methodsForGenerics = Decode.succeed ${decl.name} $body
            """
    }
  }

  private def decodeSumType(name: String, prod: Prod, customTypeReplacements: Map[Ref, TypeReplacement]): String = {
    val refName  = Ref(name)
    val mainType = customTypeReplacements.get(refName).map(_.newType).getOrElse(name)

    val paramsDecoder =
      prod.fields.map { case (name, tpe) => decodeField(name, tpe, customTypeReplacements) }.mkString(" |> ")

    // consider case objects vs case classes
    val bodyDecoder =
      if (paramsDecoder.isEmpty) s"Decode.succeed $mainType"
      else s"Decode.succeed $mainType |> $paramsDecoder"

    s""""$mainType" -> $bodyDecoder"""
  }

  private def decodeType(tpe: Type, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    tpe match {
      case r @ Ref(id, _)  => customTypeReplacements.get(r).flatMap(_.decoder).getOrElse(s"""(Decode.lazy (\\_ -> decoder$id))""")
      case Str             => "Decode.string"
      case Chr             => "Decode.string"
      case Intr            => "Decode.int"
      case Real            => "Decode.float"
      case Bool            => "Decode.bool"
      case Opt(optTpe)     => "(Decode.maybe " + decodeType(optTpe, customTypeReplacements) + ")"
      case Arr(arrTpe)     => "(Decode.list " + decodeType(arrTpe, customTypeReplacements) + ")"
      case Dict(Str, vTpe) => "(Decode.dict " + decodeType(vTpe, customTypeReplacements) + ")"
      // The Elm standard library only provides JSON decoders for dictionaries with string keys:
      case _: Dict      => throw new IllegalArgumentException("Cannot create a JsonDecoder for a Dict with anything other than String keys")
      case Prod(fields) => fields.map { case (name, tpe) => decodeField(name, tpe, customTypeReplacements) }.mkString("|> ", " |> ", "")
      case _: Sum       => throw new IllegalArgumentException("SumOfProducts jsonEncoder: we should never be here")
    }

  private def decodeField(name: String, tpe: Type, customTypeReplacements: Map[Ref, TypeReplacement]): String = {
    def decode(tpe: Type) =
      s"""required "${name}" ${decodeType(tpe, customTypeReplacements)}"""

    tpe match {
      case Opt(optTpe) =>
        s"""optional "${name}" (Decode.maybe ${decodeType(optTpe, customTypeReplacements)}) Nothing"""
      case other => decode(other)
    }
  }
}
