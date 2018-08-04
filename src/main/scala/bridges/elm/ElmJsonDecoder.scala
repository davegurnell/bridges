package bridges.elm

import bridges.core._
import bridges.core.Type._
import unindent._

trait ElmJsonDecoder {
  def decoder(decls: List[Declaration], customTypeReplacements: Map[Ref, TypeReplacement]): String =
    decls.map(decoder(_, customTypeReplacements)).mkString("\n\n")

  def decoder(decl: Declaration, customTypeReplacements: Map[Ref, TypeReplacement] = Map.empty): String =
    decl.tpe match {
      case Union(types) ⇒
        // DO NOT REMOVE SPACE AT END - needed for Elm compiler and to pass tests. Yup, dirty, I know!
        val body = types.map(decodeType(_, customTypeReplacements)).mkString("\n      ")
        i"""
            decoder${decl.id} : Decode.Decoder ${decl.id}
            decoder${decl.id} = Decode.field "type" Decode.string |> Decode.andThen decoder${decl.id}Tpe

            decoder${decl.id}Tpe : String -> Decode.Decoder ${decl.id}
            decoder${decl.id}Tpe tpe =
               case tpe of
                  $body
                  _ -> Decode.fail ("Unexpected type for ${decl.id}: " ++ tpe)
            """
      case other ⇒
        val body = decodeType(other, customTypeReplacements)

        i"""
            decoder${decl.id} : Decode.Decoder ${decl.id}
            decoder${decl.id} = decode ${decl.id} $body
            """
    }

  private def decodeType(tpe: Type, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    tpe match {
      case r @ Ref(id)        => customTypeReplacements.get(r).map(_.decoder).getOrElse(s"""(Decode.lazy (\\_ -> decoder$id))""")
      case StrLiteral(_)      => ""
      case CharLiteral(_)     => ""
      case NumLiteral(_)      => ""
      case FloatingLiteral(_) => ""
      case BoolLiteral(_)     => ""
      case Str                => "Decode.string"
      case Character          => "Decode.string"
      case Num                => "Decode.int"
      case Floating           => "Decode.float"
      case Bool               => "Decode.bool"
      case Optional(optTpe) =>
        "(Decode.maybe " + decodeType(optTpe, customTypeReplacements) + ")"
      case Array(arrTpe) => "(Decode.list " + decodeType(arrTpe, customTypeReplacements) + ")"
      case Struct(fields) =>
        fields.map(decodeField(_, customTypeReplacements)).mkString("|> ", " |> ", "")
      case Union(_) => ""
      case Intersection(key, _, fields) =>
        val mainType =
          key.fields
            .collectFirst { case (_, StrLiteral(name)) ⇒ name }
            .getOrElse("<Missing main type>")
        val paramsDecoder =
          fields.fields.map(decodeField(_, customTypeReplacements)).mkString(" |> ")

        // consider case objects vs case classes
        val bodyDecoder =
          if (paramsDecoder.isEmpty) s"Decode.succeed $mainType"
          else s"decode $mainType |> $paramsDecoder"

        s""""$mainType" -> $bodyDecoder"""
    }

  private def decodeField(field: (String, Type), customTypeReplacements: Map[Ref, TypeReplacement]): String = {
    val fieldName = field._1
    def decode(tpe: Type) =
      s"""required "$fieldName" ${decodeType(tpe, customTypeReplacements)}"""

    field._2 match {
      case Optional(optTpe) ⇒
        s"""optional "$fieldName" (Decode.maybe ${decodeType(optTpe, customTypeReplacements)}) Nothing"""
      case other ⇒ decode(other)
    }
  }
}
