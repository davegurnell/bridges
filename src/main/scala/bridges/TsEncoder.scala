package bridges

import scala.compiletime.*
import scala.deriving.*

trait TsEncoder[A]:
  def encode: TsType

  override def toString: String =
    s"TsEncoder(${encode.toString})"

object TsEncoder extends TsEncoderInstances2:
  def apply[A](using encoder: TsEncoder[A]): TsEncoder[A] =
    encoder

  def encode[A](using encoder: TsEncoder[A]): TsType =
    encoder.encode

trait TsEncoderInstances2 extends TsEncoderInstances1:
  given stringEncoder: TsEncoder[String] =
    instance(TsType.Str)

  given charEncoder: TsEncoder[Char] =
    instance(TsType.Str)

  given intEncoder: TsEncoder[Int] =
    instance(TsType.Num)

  given longEncoder: TsEncoder[Long] =
    instance(TsType.Num)

  given doubleEncoder: TsEncoder[Double] =
    instance(TsType.Num)

  given floatEncoder: TsEncoder[Float] =
    instance(TsType.Num)

  given bigDecimalEncoder: TsEncoder[BigDecimal] =
    instance(TsType.Num)

  given booleanEncoder: TsEncoder[Boolean] =
    instance(TsType.Bool)

  given optionEncoder[A](using aEnc: RefEncoder[A]): TsEncoder[Option[A]] =
    instance(TsType.Union(List(aEnc.encode, TsType.Null)))

  given mapTsEncoder[A, B](using aEnc: RefEncoder[A], bEnc: RefEncoder[B]): TsEncoder[Map[A, B]] =
    instance(TsType.Record(aEnc.encode, bEnc.encode))

  given traversableEncoder[F[x] <: Iterable[x], A](using enc: RefEncoder[A]): TsEncoder[F[A]] =
    instance(TsType.Arr(enc.encode))

trait TsEncoderInstances1 extends TsEncoderConstructors:
  private inline def elementNames[T <: Tuple]: List[String] =
    constValueTuple[T].toList.asInstanceOf[List[String]]

  private inline def optionalFlags[T <: Tuple]: List[Boolean] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => optionalFlag[t] :: optionalFlags[ts]
    }

  private inline def optionalFlag[A]: Boolean =
    inline erasedValue[A] match {
      case _: Option[?] => true
      case _            => false
    }

  inline def derived[A](using mirror: Mirror.Of[A]): TsEncoder[A] =
    val elemNames: List[String] =
      elementNames[mirror.MirroredElemLabels]

    val elemTypes: List[TsType] =
      RefEncoder.summonOrDeriveAll[mirror.MirroredElemTypes].map(_.encode)

    inline mirror match {
      case _: Mirror.ProductOf[A] =>
        val elemFlags: List[Boolean] =
          optionalFlags[mirror.MirroredElemTypes]

        instance(TsType.Struct(
          elemNames
            .zip(elemTypes)
            .zip(elemFlags)
            .map { case ((name, tpe), optional) => TsField(name, tpe, optional) }
        ))

      case _: Mirror.SumOf[A] =>
        instance(TsType.Union(
          elemNames
            .zip(elemTypes)
            .map { case (name, tpe) => TsType.discriminated(name, tpe) }
        ))
    }

trait TsEncoderConstructors:
  def instance[A](tpe: => TsType): TsEncoder[A] =
    new TsEncoder[A]:
      def encode: TsType = tpe
