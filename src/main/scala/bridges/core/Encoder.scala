package bridges.core

import bridges.core.syntax._
import eu.timepit.refined.api._
import scala.language.higherKinds
import scala.reflect.runtime.universe.WeakTypeTag
import shapeless._
import shapeless.labelled.FieldType

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

object Encoder extends EncoderInstances2

trait EncoderInstances2 extends EncoderInstances1 {
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

  implicit def traversableEncoder[F[_] <: Iterable[_], A](implicit enc: BasicEncoder[A]): BasicEncoder[F[A]] =
    pure(Arr(enc.encode))

  implicit def valueClassEncoder[A <: AnyVal, B](implicit unwrapped: Unwrapped.Aux[A, B], encoder: BasicEncoder[B]): BasicEncoder[A] =
    pure(encoder.encode)

  implicit def refinedEncoder[A, B](implicit enc: BasicEncoder[A]): BasicEncoder[Refined[A, B]] =
    Encoder.pure(enc.encode)

}

trait EncoderInstances1 extends EncoderInstances0 {
  import Type._

  implicit val hnilProdEncoder: ProdEncoder[HNil] =
    pureProd(Prod(Nil))

  implicit def hconsProdEncoder[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hEnc: Lazy[BasicEncoder[H]],
    tEnc: ProdEncoder[T]
  ): ProdEncoder[FieldType[K, H] :: T] = {
    val name = witness.value.name
    val head = hEnc.value.encode
    val tail = tEnc.encode
    pureProd(Prod((name -> head) +: tail.fields))
  }

  implicit def cnilSumEncoder: SumEncoder[CNil] =
    pureSum(Sum(Nil))

  implicit def cconsSumEncoder[K <: Symbol, H, T <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    hEnc: Lazy[ProdEncoder[H]],
    tEnc: SumEncoder[T]
  ): SumEncoder[FieldType[K, H] :+: T] = {
    val name    = witness.value.name
    val product = hEnc.value.encode
    val tail    = tEnc.encode
    pureSum(Sum((name -> product) +: tail.products))
  }

  implicit def genericProdEncoder[A, R](
    implicit
    gen: LabelledGeneric.Aux[A, R],
    enc: Lazy[ProdEncoder[R]]
  ): ProdEncoder[A] =
    pureProd(enc.value.encode)

  implicit def genericSumEncoder[A, R](
    implicit
    gen: LabelledGeneric.Aux[A, R],
    enc: Lazy[SumEncoder[R]]
  ): SumEncoder[A] =
    pureSum(enc.value.encode)
}

trait EncoderInstances0 extends EncoderConstructors {
  import Type._

  implicit def genericBasicEncoder[A](
    implicit
    low: LowPriority,
    tpeTag: WeakTypeTag[A]
  ): BasicEncoder[A] =
    pure(Ref(getCleanTagName[A]))
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
