package bridges.elm

import bridges.core._
import bridges.core.Type._
import unindent._

trait ElmJsonEncoder {
  def encoder(decls: List[Decl], customTypeReplacements: Map[Ref, TypeReplacement]): String =
    decls.map(encoder(_, customTypeReplacements)).mkString("\n\n")

  def encoder(decl: Decl, customTypeReplacements: Map[Ref, TypeReplacement] = Map.empty): String =
    decl.tpe match {
      case Sum(products) ⇒
        // DO NOT REMOVE SPACE AT END - needed for Elm compiler and to pass tests. Yup, dirty, I know!
        val body =
          products.map(encodeSumType(_, customTypeReplacements)).mkString("\n      ")

        i"""
            encoder${decl.name} : ${decl.name} -> Encode.Value
            encoder${decl.name} tpe =
               case tpe of
                  $body
            """
      case other ⇒
        val body = encodeType(other, "obj", decl.name, customTypeReplacements)

        i"""
            encoder${decl.name} : ${decl.name} -> Encode.Value
            encoder${decl.name} obj = $body
            """
    }

  private def encodeSumType(prod: DeclF[Type.Prod], customTypeReplacements: Map[Ref, TypeReplacement]): String = {
    val refName  = Ref(prod.name)
    val mainType = customTypeReplacements.get(refName).map(_.newType).getOrElse(prod.name)

    val params        = prod.tpe.fields.map(_.name).mkString(" ")
    val paramsEncoder = prod.tpe.fields.map(encodeField(_, "", customTypeReplacements))

    val caseEncoder = if (params.isEmpty) mainType else s"$mainType $params"
    val bodyEncoder =
      (paramsEncoder :+ s"""("type", Encode.string "$mainType")""")
        .mkString("Encode.object [ ", ", ", " ]")

    s"""$caseEncoder -> $bodyEncoder"""
  }

  private def encodeType(tpe: Type, objectName: String, fieldName: String, customTypeReplacements: Map[Ref, TypeReplacement]): String =
    tpe match {
      case r @ Ref(id) => customTypeReplacements.get(r).map(_.encoder).getOrElse(s"encoder$id") + s" $fieldName"
      case Str         => s"Encode.string $fieldName"
      case Chr         => s"Encode.string $fieldName"
      case Intr        => s"Encode.int $fieldName"
      case Real        => s"Encode.float $fieldName"
      case Bool        => s"Encode.bool $fieldName"
      case Opt(optTpe) =>
        "Maybe.withDefault Encode.null (Maybe.map " + encodeType(optTpe, objectName, fieldName, customTypeReplacements) + ")"
      case Arr(arrTpe)  => "Encode.list (List.map " + encodeType(arrTpe, objectName, fieldName, customTypeReplacements) + ")"
      case Prod(fields) => fields.map(encodeField(_, objectName, customTypeReplacements)).mkString("Encode.object [ ", ", ", " ]")
      case _: Sum       => throw new IllegalArgumentException("SumOfProducts jsonEncoder: we should never be here")
    }

  private def encodeField(field: Decl, objectName: String, customTypeReplacements: Map[Ref, TypeReplacement]): String = {
    val typeFieldName =
      if (objectName.isEmpty) field.name else s"$objectName.${field.name}"

    val encoding = encodeType(field.tpe, objectName, typeFieldName, customTypeReplacements)

    s"""("${field.name}", $encoding)"""
  }

}
