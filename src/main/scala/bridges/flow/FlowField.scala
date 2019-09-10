package bridges.flow

import bridges.core._
import bridges.flow.syntax._

final case class FlowField(name: String, valueType: FlowType, optional: Boolean = false)

object FlowField {
  implicit val rename: Rename[FlowField] =
    Rename.pure { (field, from, to) =>
      val FlowField(name, valueType, optional) = field

      FlowField(
        name = if (field.name == from) to else field.name,
        valueType = valueType.rename(from, to),
        optional = optional
      )
    }
}

final case class FlowRestField(name: String, keyType: FlowType, valueType: FlowType)

object FlowRestField {
  implicit val rename: Rename[FlowRestField] =
    Rename.pure { (field, from, to) =>
      val FlowRestField(name, keyType, valueType) = field

      FlowRestField(
        name = if (field.name == from) to else field.name,
        keyType = keyType.rename(from, to),
        valueType = valueType.rename(from, to)
      )
    }
}
