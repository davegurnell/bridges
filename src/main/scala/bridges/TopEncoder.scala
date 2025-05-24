package bridges

import scala.deriving.*

trait TopEncoder[A]:
  def encode: TsType

  override def toString: String =
    s"TopEncoder(${encode.toString})"

object TopEncoder extends TopEncoderInstances2:
  def apply[A](using encoder: TopEncoder[A]): TopEncoder[A] =
    encoder

  def encode[A](using encoder: TopEncoder[A]): TsType =
    encoder.encode

trait TopEncoderInstances2 extends TopEncoderInstances1:
  given fromTsEncoder[A: TsEncoder]: TopEncoder[A] =
    instance(TsEncoder[A].encode)

trait TopEncoderInstances1 extends TopEncoderConstructors:
  inline given derived[A: Mirror.Of]: TopEncoder[A] =
    instance(TsEncoder.derived[A].encode)

trait TopEncoderConstructors:
  def instance[A](tpe: => TsType): TopEncoder[A] =
    new TopEncoder[A]:
      def encode: TsType = tpe
