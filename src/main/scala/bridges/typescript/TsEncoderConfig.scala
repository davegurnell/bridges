package bridges.typescript

case class TsEncoderConfig(
    optionalFields: Boolean = true,
    refsInUnions: Boolean = false
)

object TsEncoderConfig {
  implicit val default: TsEncoderConfig =
    TsEncoderConfig()
}
