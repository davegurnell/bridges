package bridges.core

import eu.timepit.refined.api._

trait Encoder[A] {
  def encode: Type
}

trait ProdEncoder[A] extends Encoder[A] {
  override def encode: Type.Prod
}

trait SumEncoder[A] extends Encoder[A] {
  override def encode: Type.Sum
}

trait BasicEncoder[A] extends Encoder[A]

object Encoder extends DerivedEncoderInstances1 {
  import Type._

  implicit val stringEncoder: BasicEncoder[String] =
    pure(Str)

  implicit val charEncoder: BasicEncoder[Char] =
    pure(Chr)

  implicit val intEncoder: BasicEncoder[Int] =
    pure(Intr)

  implicit val longEncoder: BasicEncoder[Long] =
    pure(Intr)

  implicit val bigDecimalEncoder: BasicEncoder[BigDecimal] =
    pure(Real)

  implicit val doubleEncoder: BasicEncoder[Double] =
    pure(Real)

  implicit val floatEncoder: BasicEncoder[Float] =
    pure(Real)

  implicit val booleanEncoder: BasicEncoder[Boolean] =
    pure(Bool)

  implicit def optionEncoder[A](implicit enc: BasicEncoder[A]): BasicEncoder[Option[A]] =
    pure(Opt(enc.encode))

  implicit def mapEncoder[A, B](implicit aEnc: BasicEncoder[A], bEnc: Encoder[B]): BasicEncoder[Map[A, B]] =
    pure(Dict(aEnc.encode, bEnc.encode))

  implicit def traversableEncoder[F[_] <: Iterable[?], A](implicit enc: BasicEncoder[A]): BasicEncoder[F[A]] =
    pure(Arr(enc.encode))

  implicit def refinedEncoder[A, B](implicit enc: BasicEncoder[A]): BasicEncoder[Refined[A, B]] =
    pure(enc.encode)

}

trait EncoderConstructors {
  import Type._

  def apply[A](implicit enc: Encoder[A]): Encoder[A] =
    enc

  def pure[A](tpe: Type): BasicEncoder[A] =
    new BasicEncoder[A] { def encode: Type = tpe }

  def pureProd[A](tpe: Prod): ProdEncoder[A] =
    new ProdEncoder[A] { def encode: Prod = tpe }

  def pureSum[A](tpe: Sum): SumEncoder[A] =
    new SumEncoder[A] { def encode: Sum = tpe }
}
