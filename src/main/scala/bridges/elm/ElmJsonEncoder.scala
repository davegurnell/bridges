package bridges.elm

import bridges.core._
import bridges.core.Type._
import unindent._

trait ElmJsonEncoder {
  def encoder(decls: List[Declaration], customTypeReplacements: Map[Ref, TypeReplacement]): String =
    decls.map(encoder(_, customTypeReplacements)).mkString("\n\n")

  def encoder(decl: Declaration, customTypeReplacements: Map[Ref, TypeReplacement] = Map.empty): String =
    decl.tpe match {
      case Union(types) ⇒
        // DO NOT REMOVE SPACE AT END - needed for Elm compiler and to pass tests. Yup, dirty, I know!
        val body =
          types.map(encodeType(_, "", decl.id, customTypeReplacements)).mkString("\n      ")

        i"""
            encoder${decl.id} : ${decl.id} -> Encode.Value
            encoder${decl.id} tpe =
               case tpe of
                  $body
            """
      case other ⇒
        val body = encodeType(other, "obj", decl.id, customTypeReplacements)

        i"""
            encoder${decl.id} : ${decl.id} -> Encode.Value
            encoder${decl.id} obj = $body
            """
    }

  private def encodeType(tpe: Type, objectName: String, fieldName: String, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    tpe match {
      case r @ Ref(id)        => customTypeReplacements.get(r).map(_.encoder).getOrElse(s"encoder$id") + s" $fieldName"
      case StrLiteral(_)      => ""
      case CharLiteral(_)     => ""
      case NumLiteral(_)      => ""
      case FloatingLiteral(_) => ""
      case BoolLiteral(_)     => ""
      case Str                => s"Encode.string $fieldName"
      case Character          => s"Encode.string $fieldName"
      case Num                => s"Encode.int $fieldName"
      case Floating           => s"Encode.float $fieldName"
      case Bool               => s"Encode.bool $fieldName"
      case Optional(optTpe) =>
        "Maybe.withDefault Encode.null (Maybe.map " + encodeType(
          optTpe,
          objectName,
          fieldName,
          customTypeReplacements
        ) + ")"
      case Array(arrTpe) =>
        "Encode.list (List.map " + encodeType(
          arrTpe,
          objectName,
          fieldName,
          customTypeReplacements
        ) + ")"
      case Struct(fields) =>
        fields
          .map(encodeField(_, objectName, customTypeReplacements))
          .mkString("Encode.object [ ", ", ", " ]")
      case Union(_) => ""
      case Intersection(key, _, fields) =>
        val mainType =
          key.fields
            .collectFirst { case (_, StrLiteral(name)) ⇒ name }
            .getOrElse("<Missing main type>")
        val params        = fields.fields.map { case (name, _) ⇒ name }.mkString(" ")
        val paramsEncoder = fields.fields.map(encodeField(_, "", customTypeReplacements))

        val caseEncoder = if (params.isEmpty) mainType else s"$mainType $params"
        val bodyEncoder =
          (paramsEncoder :+ s"""("type", Encode.string "$mainType")""")
            .mkString("Encode.object [ ", ", ", " ]")

        s"""$caseEncoder -> $bodyEncoder"""
    }

  private def encodeField(
      field: (String, Type),
      objectName: String,
      customTypeReplacements: Map[Ref, TypeReplacement]
  ): String = {
    val fieldName = field._1

    val typeFieldName =
      if (objectName.isEmpty) fieldName else s"$objectName.$fieldName"
    val encoding = encodeType(field._2, objectName, typeFieldName, customTypeReplacements)

    s"""("$fieldName", $encoding)"""
  }

}
