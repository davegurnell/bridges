package bridges.typescript

final case class TsField(name: String, valueType: TsType, optional: Boolean = false):
  def rename(from: String, to: String): TsField =
    TsField(
      name = if name == from then to else name,
      valueType = valueType.rename(from, to),
      optional = optional
    )

final case class TsRestField(name: String, keyType: TsType, valueType: TsType):
  def rename(from: String, to: String): TsRestField =
    TsRestField(
      name = if name == from then to else name,
      keyType = keyType.rename(from, to),
      valueType = valueType.rename(from, to)
    )
