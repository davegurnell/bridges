package bridges.core

import bridges.core.syntax._
// import eu.timepit.refined.api._
import scala.language.higherKinds
// import scala.reflect.runtime.universe.WeakTypeTag
// import shapeless._
// import shapeless.labelled.FieldType
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

trait Encoder[A]:
  def encode: Type

trait ProdEncoder[A] extends Encoder[A]:
  override def encode: Type.Prod

trait SumEncoder[A] extends Encoder[A]:
  override def encode: Type.Sum

trait BasicEncoder[A] extends Encoder[A]

object Encoder extends EncoderInstances

trait EncoderInstances extends EncoderDerivation:
  import Type._

  given BasicEncoder[String] =
    pure(Str)

  given BasicEncoder[Char] =
    pure(Chr)

  given BasicEncoder[Int] =
    pure(Intr)

  given BasicEncoder[Long] =
    pure(Intr)

  given BasicEncoder[BigDecimal] =
    pure(Real)

  given BasicEncoder[Double] =
    pure(Real)

  given BasicEncoder[Float] =
    pure(Real)

  given BasicEncoder[Boolean] =
    pure(Bool)

  given BasicEncoder[java.util.UUID] =
    pure(Str)

  given BasicEncoder[java.util.Date] =
    pure(Intr)

  given optionEncoder[A](using enc: BasicEncoder[A]): BasicEncoder[Option[A]] =
    pure(Opt(enc.encode))

  given mapEncoder[A, B](using aEnc: BasicEncoder[A], bEnc: Encoder[B]): BasicEncoder[Map[A, B]] =
    pure(Dict(aEnc.encode, bEnc.encode))

  given traversableEncoder[F[_] <: Iterable[_], A](using enc: BasicEncoder[A]): BasicEncoder[F[A]] =
    pure(Arr(enc.encode))

  // given valueClassEncoder[A <: AnyVal, B](using unwrapped: Unwrapped.Aux[A, B], encoder: BasicEncoder[B]): BasicEncoder[A] =
  //   pure(encoder.encode)

  // given refinedEncoder[A, B](using enc: BasicEncoder[A]): BasicEncoder[Refined[A, B]] =
  //   Encoder.pure(enc.encode)

trait EncoderDerivation extends EncoderConstructors:
  import Type._

  inline def summonEncoders[A <: Tuple]: List[Encoder[_]] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (h *: t) => summonInline[Encoder[h]] :: summonEncoders[t]

  inline def summonProdEncoders[A <: Tuple]: List[ProdEncoder[_]] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (h *: t) => summonInline[ProdEncoder[h]] :: summonProdEncoders[t]

  inline def summonLabels[A <: Tuple]: List[String] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].toString :: summonLabels[t]

  inline given derived[A](using mirror: Mirror.Of[A]): Encoder[A] =
    val labels = summonLabels[mirror.MirroredElemLabels]
    inline mirror match
      case mirror: Mirror.SumOf[A] =>     sumEncoder(labels, summonProdEncoders[mirror.MirroredElemTypes])
      case mirror: Mirror.ProductOf[A] => productEncoder(labels, summonEncoders[mirror.MirroredElemTypes])

  inline given derivedProd[A](using mirror: Mirror.ProductOf[A]): ProdEncoder[A] =
    val labels = summonLabels[mirror.MirroredElemLabels]
    productEncoder(labels, summonEncoders[mirror.MirroredElemTypes])

  def sumEncoder[A](labels: List[String], encoders: => List[ProdEncoder[_]]): SumEncoder[A] =
    pureSum(Sum(
      labels
        .zip(encoders.iterator)
        .map { case (label, encoder) => label -> encoder.encode }
    ))

  def productEncoder[A](labels: List[String], encoders: => List[Encoder[_]]): ProdEncoder[A] =
    pureProd(Prod(
      labels
        .zip(encoders.iterator)
        .map { case (label, encoder) => label -> encoder.encode }
    ))

trait EncoderConstructors:
  import Type._

  def apply[A](using enc: Encoder[A]): Encoder[A] =
    enc

  def pure[A](tpe: Type): BasicEncoder[A] =
    new BasicEncoder[A]:
      def encode: Type = tpe

  def pureProd[A](tpe: Prod): ProdEncoder[A] =
    new ProdEncoder[A]:
      def encode: Prod = tpe

  def pureSum[A](tpe: Sum): SumEncoder[A] =
    new SumEncoder[A]:
      def encode: Sum = tpe

