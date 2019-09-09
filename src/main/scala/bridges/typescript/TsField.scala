package bridges.typescript

import bridges.core._
import bridges.typescript.syntax._

final case class TsField(name: String, valueType: TsType, optional: Boolean = false)

object TsField {
  implicit val rename: Rename[TsField] =
    Rename.pure { (field, from, to) =>
      val TsField(name, valueType, optional) = field

      TsField(
        name = if (field.name == from) to else field.name,
        valueType = valueType.rename(from, to),
        optional = optional
      )
    }
}

final case class TsRestField(name: String, keyType: TsType, valueType: TsType)

object TsRestField {
  implicit val rename: Rename[TsRestField] =
    Rename.pure { (field, from, to) =>
      val TsRestField(name, keyType, valueType) = field

      TsRestField(
        name = if (field.name == from) to else field.name,
        keyType = keyType.rename(from, to),
        valueType = valueType.rename(from, to)
      )
    }
}
