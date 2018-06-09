package bridges

import bridges.Type._
import unindent._

trait JsonDecoder[A] {
  def decoder(decl: Declaration): String
  def decoder(decls: List[Declaration]): String =
    decls.map(decoder).mkString("\n\n")
}

trait ElmJsonDecoder extends JsonDecoder[Elm] {

  def decoder(decl: Declaration): String = {
    decl.tpe match {
      case Union(types) ⇒
        // DO NOT REMOVE SPACE AT END - needed for Elm compiler and to pass tests. Yup, dirty, I know!
        val body = types.map(decodeType).mkString("\n      ")
        i"""
           decoder : Decode.Decoder ${decl.id}
           decoder = field "type" string |> Decode.andThen decoder${decl.id}

           decoder${decl.id} : String -> Decode.Decoder ${decl.id}
           decoder${decl.id} tpe =
              case tpe of
                 $body
                 _ -> Decode.fail ("Unexpected type for ${decl.id}")
           """
      case other ⇒
        val body = decodeType(other)

        i"""
           decoder : Decode.Decoder ${decl.id}
           decoder = decode ${decl.id} $body
           """
    }
  }

  def decodeType(tpe: Type): String =
    tpe match {
      case Ref(id)            => s"$id.decoder"
      case StrLiteral(_)      => ""
      case CharLiteral(_)     => ""
      case NumLiteral(_)      => ""
      case FloatingLiteral(_) => ""
      case BoolLiteral(_)     => ""
      case Str                => s"Decode.string"
      case Character          => s"Decode.string"
      case Num                => s"Decode.int"
      case Floating           => s"Decode.float"
      case Bool               => s"Decode.bool"
      case Optional(optTpe)   => "(Decode.maybe " + decodeType(optTpe) + ")"
      case Array(arrTpe)      => "(Decode.list " + decodeType(arrTpe) + ")"
      case Struct(fields) =>
        fields.map(decodeField).mkString("|> ", " |> ", "")
      case Union(_) => ""
      case Intersection(types) =>
        println(types)
        val mainType = types.collect { case Ref(tpeId) ⇒ tpeId }.mkString
        val paramsDecoder = types
          .collect {
            case Struct(fields) ⇒
              fields
                .collect {
                  case (key, Struct(flds)) if key == "fields" ⇒ flds
                }
                .flatten
                .map(decodeField)
          }
          .flatten
          .mkString(" |> ")

        // consider case objects vs case classes
        val bodyDecoder =
          if (paramsDecoder.isEmpty) s"Decode.success $mainType"
          else s"decode $mainType |> $paramsDecoder"

        s""""$mainType" -> $bodyDecoder"""
    }

  def decodeField(field: (String, Type)): String = {
    val fieldName = field._1
    def decode(tpe: Type) = s"""required "$fieldName" ${decodeType(tpe)}"""

    field._2 match {
      case Optional(optTpe) ⇒ s"""Decode.maybe (${decode(optTpe)})"""
      case other ⇒ decode(other)
    }
  }
}

object JsonDecoder {
  implicit object ElmJsonDecoder extends ElmJsonDecoder
}
