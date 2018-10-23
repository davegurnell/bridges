package bridges.flow

import bridges.core.Encoder

trait FlowEncoder[A] {
  def encode: FlowType
}

object FlowEncoder {
  def apply[A](implicit encoder: FlowEncoder[A]): FlowEncoder[A] =
    encoder

  def pure[A](tpe: FlowType): FlowEncoder[A] =
    new FlowEncoder[A] {
      override val encode = tpe
    }

  implicit def from[A](implicit encoder: Encoder[A]): FlowEncoder[A] =
    pure(FlowType.from(encoder.encode))
}
