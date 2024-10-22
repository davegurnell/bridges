package bridges.typescript

case class TsEncoderConfig(
    optionalFields: Boolean = true,
    refsInUnions: Boolean = false
)

object TsEncoderConfig {
  given default: TsEncoderConfig =
    TsEncoderConfig()
}
