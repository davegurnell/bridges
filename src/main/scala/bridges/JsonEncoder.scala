package bridges

import bridges.Type._
import unindent._

trait JsonEncoder[A] {
  def encoder(decl: Declaration): String
  def encoder(decls: List[Declaration]): String =
    decls.map(encoder).mkString("\n\n")
}

trait ElmJsonEncoder extends JsonEncoder[Elm] {

  def encoder(decl: Declaration): String = {
    decl.tpe match {
      case Union(types) ⇒
        // DO NOT REMOVE SPACE AT END - needed for Elm compiler and to pass tests. Yup, dirty, I know!
        val body =
          types.map(encodeType(decl.id, _, "", decl.id)).mkString("\n      ")

        i"""
           encoder : ${decl.id} -> Encode.Value
           encoder tpe =
              case tpe of
                 $body
           """
      case other ⇒
        val body = encodeType(decl.id, other, "obj", decl.id)

        i"""
        encoder : ${decl.id} -> Encode.Value
        encoder obj = $body
        """
    }
  }

  def encodeType(
      topType: String,
      tpe: Type,
      objectName: String,
      fieldName: String
  ): String =
    tpe match {
      case Ref(id) =>
        if (id == topType) s"encoder $fieldName" else s"$id.encoder $fieldName"
      case StrLiteral(_)      => ""
      case CharLiteral(_)     => ""
      case NumLiteral(_)      => ""
      case FloatingLiteral(_) => ""
      case BoolLiteral(_)     => ""
      case UUIDLiteral(_)     => ""
      case Str                => s"Encode.string $fieldName"
      case Character          => s"Encode.string $fieldName"
      case Num                => s"Encode.int $fieldName"
      case Floating           => s"Encode.float $fieldName"
      case Bool               => s"Encode.bool $fieldName"
      case UUIDType           => s"Uuid.encode $fieldName"
      case Optional(optTpe) =>
        "Maybe.withDefault Encode.null (Maybe.map " + encodeType(
          topType,
          optTpe,
          objectName,
          fieldName
        ) + ")"
      case Array(arrTpe) =>
        "Encode.list (List.map " + encodeType(
          topType,
          arrTpe,
          objectName,
          fieldName
        ) + ")"
      case Struct(fields) =>
        fields
          .map(encodeField(topType, _, objectName))
          .mkString("Encode.object [ ", ", ", " ]")
      case Union(_) => ""
      case Intersection(key, _, fields) =>
        val mainType =
          key.fields
            .collectFirst { case (_, StrLiteral(name)) ⇒ name }
            .getOrElse("<Missing main type>")
        val params = fields.fields.map { case (name, _) ⇒ name }.mkString(" ")
        val paramsEncoder = fields.fields.map(encodeField(topType, _, ""))

        val caseEncoder = if (params.isEmpty) mainType else s"$mainType $params"
        val bodyEncoder =
          (paramsEncoder :+ s"""("type", Encode.string "$mainType")""")
            .mkString("Encode.object [ ", ", ", " ]")

        s"""$caseEncoder -> $bodyEncoder"""
    }

  def encodeField(
      topType: String,
      field: (String, Type),
      objectName: String
  ): String = {
    val fieldName = field._1

    val typeFieldName =
      if (objectName.isEmpty) fieldName else s"$objectName.$fieldName"
    val encoding = encodeType(topType, field._2, objectName, typeFieldName)

    s"""("$fieldName", $encoding)"""
  }
}

object JsonEncoder {
  implicit object ElmJsonEncoder extends ElmJsonEncoder
}
