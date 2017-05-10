package bridges.ts

trait EncoderSyntax {
  import Type._

  def encode[A: Encoder]: Type =
    Encoder[A].encode

  def bindings[A: Encoder]: List[Binding] =
    Encoder[A].encode.bindings
}
