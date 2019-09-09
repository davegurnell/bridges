package bridges.flow

case class FlowEncoderConfig(optionalFields: Boolean)

object FlowEncoderConfig {
  implicit val default: FlowEncoderConfig =
    FlowEncoderConfig(optionalFields = false)
}
