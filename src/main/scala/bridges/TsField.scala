package bridges

final case class TsField(name: String, valueType: TsType, optional: Boolean = false)

object TsField {
  implicit val rename: Rename[TsField] =
    Rename.instance { (field, from, to) =>
      val TsField(name, valueType, optional) = field

      TsField(
        name      = if (field.name == from) to else field.name,
        valueType = Rename.rename(valueType, from, to),
        optional  = optional
      )
    }
}

final case class TsRestField(name: String, keyType: TsType, valueType: TsType)

object TsRestField {
  implicit val rename: Rename[TsRestField] =
    Rename.instance { (field, from, to) =>
      val TsRestField(name, keyType, valueType) = field

      TsRestField(
        name      = if (name == from) to else name,
        keyType   = Rename.rename(keyType, from, to),
        valueType = Rename.rename(valueType, from, to)
      )
    }
}
