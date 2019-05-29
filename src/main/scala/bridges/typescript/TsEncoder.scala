package bridges.typescript

import bridges.core.Encoder

trait TsEncoder[A] {
  def encode: TsType
}

object TsEncoder {
  def apply[A](implicit encoder: TsEncoder[A]): TsEncoder[A] =
    encoder

  def pure[A](tpe: TsType): TsEncoder[A] =
    new TsEncoder[A] {
      override val encode = tpe
    }

  implicit def from[A](implicit encoder: Encoder[A], config: TsEncoderConfig): TsEncoder[A] =
    pure(TsType.from(encoder.encode))
}
