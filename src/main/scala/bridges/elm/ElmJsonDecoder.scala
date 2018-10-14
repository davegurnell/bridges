package bridges.elm

import bridges.core._
import bridges.core.Type._
import unindent._

trait ElmJsonDecoder {
  def decoder(decls: List[Decl], customTypeReplacements: Map[Ref, TypeReplacement]): String =
    decls.map(decoder(_, customTypeReplacements)).mkString("\n\n")

  def decoder(decl: Decl, customTypeReplacements: Map[Ref, TypeReplacement] = Map.empty): String =
    decl.tpe match {
      case Sum(products) ⇒
//         DO NOT REMOVE SPACE AT END - needed for Elm compiler and to pass tests. Yup, dirty, I know!
        val body = products.map(decodeSumType(_, customTypeReplacements)).mkString("\n      ")
        i"""
            decoder${decl.name} : Decode.Decoder ${decl.name}
            decoder${decl.name} = Decode.field "type" Decode.string |> Decode.andThen decoder${decl.name}Tpe

            decoder${decl.name}Tpe : String -> Decode.Decoder ${decl.name}
            decoder${decl.name}Tpe tpe =
               case tpe of
                  $body
                  _ -> Decode.fail ("Unexpected type for ${decl.name}: " ++ tpe)
            """
      case other ⇒
        val body = decodeType(other, customTypeReplacements)

        i"""
            decoder${decl.name} : Decode.Decoder ${decl.name}
            decoder${decl.name} = decode ${decl.name} $body
            """
    }

  private def decodeSumType(decl: DeclF[Prod], customTypeReplacements: Map[Ref, TypeReplacement]): String = {
    val refName  = Ref(decl.name)
    val mainType = customTypeReplacements.get(refName).map(_.newType).getOrElse(decl.name)

    val paramsDecoder =
      decl.tpe.fields.map(decodeField(_, customTypeReplacements)).mkString(" |> ")

    // consider case objects vs case classes
    val bodyDecoder =
      if (paramsDecoder.isEmpty) s"Decode.succeed $mainType"
      else s"decode $mainType |> $paramsDecoder"

    s""""$mainType" -> $bodyDecoder"""
  }

  private def decodeType(tpe: Type, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    tpe match {
      case r @ Ref(id)  => customTypeReplacements.get(r).map(_.decoder).getOrElse(s"""(Decode.lazy (\\_ -> decoder$id))""")
      case Str          => "Decode.string"
      case Chr          => "Decode.string"
      case Intr         => "Decode.int"
      case Real         => "Decode.float"
      case Bool         => "Decode.bool"
      case Opt(optTpe)  => "(Decode.maybe " + decodeType(optTpe, customTypeReplacements) + ")"
      case Arr(arrTpe)  => "(Decode.list " + decodeType(arrTpe, customTypeReplacements) + ")"
      case Prod(fields) => fields.map(decodeField(_, customTypeReplacements)).mkString("|> ", " |> ", "")
      case _: Sum       => throw new IllegalArgumentException("SumOfProducts jsonEncoder: we should never be here")
    }

  private def decodeField(field: Decl, customTypeReplacements: Map[Ref, TypeReplacement]): String = {
    def decode(tpe: Type) =
      s"""required "${field.name}" ${decodeType(tpe, customTypeReplacements)}"""

    field.tpe match {
      case Opt(optTpe) ⇒
        s"""optional "${field.name}" (Decode.maybe ${decodeType(optTpe, customTypeReplacements)}) Nothing"""
      case other ⇒ decode(other)
    }
  }
}
