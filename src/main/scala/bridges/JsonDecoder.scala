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
        val body = types.map(decodeType(decl.id, _)).mkString("\n      ")
        i"""
           decoder : Decode.Decoder ${decl.id}
           decoder = Decode.field "type" Decode.string |> Decode.andThen decoder${decl.id}

           decoder${decl.id} : String -> Decode.Decoder ${decl.id}
           decoder${decl.id} tpe =
              case tpe of
                 $body
                 _ -> Decode.fail ("Unexpected type for ${decl.id}")
           """
      case other ⇒
        val body = decodeType(decl.id, other)

        i"""
           decoder : Decode.Decoder ${decl.id}
           decoder = decode ${decl.id} $body
           """
    }
  }

  def decodeType(topType: String, tpe: Type): String =
    tpe match {
      case Ref(id)            => if (id == topType) "decoder" else s"$id.decoder"
      case StrLiteral(_)      => ""
      case CharLiteral(_)     => ""
      case NumLiteral(_)      => ""
      case FloatingLiteral(_) => ""
      case BoolLiteral(_)     => ""
      case UUIDLiteral(_)     => ""
      case Str                => "Decode.string"
      case Character          => "Decode.string"
      case Num                => "Decode.int"
      case Floating           => "Decode.float"
      case Bool               => "Decode.bool"
      case UUIDType           => "Uuid.decoder"
      case Optional(optTpe) =>
        "(Decode.maybe " + decodeType(topType, optTpe) + ")"
      case Array(arrTpe) => "(Decode.list " + decodeType(topType, arrTpe) + ")"
      case Struct(fields) =>
        fields.map(decodeField(topType, _)).mkString("|> ", " |> ", "")
      case Union(_) => ""
      case Intersection(key, _, fields) =>
        val mainType =
          key.fields
            .collectFirst { case (_, StrLiteral(name)) ⇒ name }
            .getOrElse("<Missing main type>")
        val paramsDecoder =
          fields.fields.map(decodeField(topType, _)).mkString(" |> ")

        // consider case objects vs case classes
        val bodyDecoder =
          if (paramsDecoder.isEmpty) s"Decode.succeed $mainType"
          else s"decode $mainType |> $paramsDecoder"

        s""""$mainType" -> $bodyDecoder"""
    }

  def decodeField(topType: String, field: (String, Type)): String = {
    val fieldName = field._1
    def decode(tpe: Type) =
      s"""required "$fieldName" ${decodeType(topType, tpe)}"""

    field._2 match {
      case Optional(optTpe) ⇒ s"""Decode.maybe (${decode(optTpe)})"""
      case other ⇒ decode(other)
    }
  }
}

object JsonDecoder {
  implicit object ElmJsonDecoder extends ElmJsonDecoder
}
