package bridges.typescript

case class TsEncoderConfig(optionalFields: Boolean)

object TsEncoderConfig {
  implicit val default: TsEncoderConfig =
    TsEncoderConfig(optionalFields = true)
}
