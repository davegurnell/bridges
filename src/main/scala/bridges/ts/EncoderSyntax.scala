package bridges.ts

trait EncoderSyntax {
  import TsType._

  def encode[A: Encoder]: TsType =
    Encoder[A].encode

  def bindings[A: Encoder]: List[Binding] =
    Encoder[A].encode.bindings
}
