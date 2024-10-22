package bridges.typescript

import bridges.typescript.syntax.*
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*
import scala.util.NotGiven

trait TsEncoder[A]:
  def encode: TsType

trait BasicEncoder[A] extends TsEncoder[A]

trait AdtEncoder[A] extends TsEncoder[A]

trait StructEncoder[A] extends AdtEncoder[A]:
  override def encode: TsType.Struct

trait UnionEncoder[A] extends AdtEncoder[A]:
  override def encode: TsType.Union

object TsEncoder extends TsEncoderInstances, TsEncoderConstructors

trait TsEncoderInstances extends DerivedTsEncoderInstances:
  self: TsEncoderConstructors =>

  given stringEncoder: BasicEncoder[String] =
    basicEnc(TsType.Str)

  given charEncoder: BasicEncoder[Char] =
    basicEnc(TsType.Chr)

  given intEncoder: BasicEncoder[Int] =
    basicEnc(TsType.Intr)

  given longEncoder: BasicEncoder[Long] =
    basicEnc(TsType.Intr)

  given bigDecimalEncoder: BasicEncoder[BigDecimal] =
    basicEnc(TsType.Real)

  given doubleEncoder: BasicEncoder[Double] =
    basicEnc(TsType.Real)

  given floatEncoder: BasicEncoder[Float] =
    basicEnc(TsType.Real)

  given booleanEncoder: BasicEncoder[Boolean] =
    basicEnc(TsType.Bool)

  given optionEncoder[A](using enc: BasicEncoder[A]): BasicEncoder[Option[A]] =
    basicEnc(TsType.nullable(enc.encode))

  given mapEncoder[A, B](using aEnc: BasicEncoder[A], bEnc: TsEncoder[B]): BasicEncoder[Map[A, B]] =
    basicEnc(TsType.Struct(Nil, Some(TsRestField("rest", aEnc.encode, bEnc.encode))))

  given traversableEncoder[F[_] <: Iterable[?], A](using enc: BasicEncoder[A]): BasicEncoder[F[A]] =
    basicEnc(TsType.Arr(enc.encode))

trait DerivedTsEncoderInstances extends LowPriorityEncoderInstances:
  self: TsEncoderConstructors =>

  inline given allBasicEncoders[A <: Tuple]: List[BasicEncoder[Any]] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[BasicEncoder[h]].asInstanceOf[BasicEncoder[Any]] :: allBasicEncoders[t]

  inline given allStructEncoders[A <: Tuple]: List[StructEncoder[Any]] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[StructEncoder[h]].asInstanceOf[StructEncoder[Any]] :: allStructEncoders[t]

  private inline def allLabels[A <: Tuple]: List[String] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].asInstanceOf[String] :: allLabels[t]

  inline given deriveStructEncoder[A](using mirror: Mirror.ProductOf[A], config: TsEncoderConfig): StructEncoder[A] =
    lazy val labels: List[String] = allLabels[mirror.MirroredElemLabels]
    lazy val types: List[TsType]  = allBasicEncoders[mirror.MirroredElemTypes].map(_.encode)
    structEnc {
      TsType.Struct(
        labels.zip(types).map { (name, tpe) =>
          TsField(name, tpe, fieldIsOptional(config, tpe))
        }
      )
    }

  inline given deriveUnionEncoder[A](using mirror: Mirror.SumOf[A], config: TsEncoderConfig): UnionEncoder[A] =
    import TsType.*

    lazy val labels: List[String]       = allLabels[mirror.MirroredElemLabels]
    lazy val types: List[TsType.Struct] = allStructEncoders[mirror.MirroredElemTypes].map(_.encode)

    unionEnc {
      val products: List[TsType] =
        labels.zip(types).map { (label, tpe) =>
          if config.refsInUnions then
            intersect(
              TsType.struct(TsField("type", StrLit(label))),
              Ref(label)
            )
          else
            TsType.Struct(
              TsField("type", StrLit(label)) ::
                tpe.fields.map(field => field.copy(optional = fieldIsOptional(config, field.valueType)))
            )
        }

      TsType.Union(products)
    }

  private def fieldIsOptional(config: TsEncoderConfig, tpe: TsType) =
    config.optionalFields && tpe.isNullable

trait LowPriorityEncoderInstances:
  self: TsEncoderConstructors =>

  inline given basicRefEncoder[A]: BasicEncoder[A] =
    lazy val name = TypeName.of[A]
    basicEnc(TsType.Ref(name))

trait TsEncoderConstructors:
  def apply[A](implicit encoder: TsEncoder[A]): TsEncoder[A] =
    encoder

  def basicEnc[A](tpe: TsType): BasicEncoder[A] =
    new BasicEncoder[A]:
      override def encode: TsType =
        tpe

  def structEnc[A](tpe: TsType.Struct): StructEncoder[A] =
    new StructEncoder[A]:
      override def encode: TsType.Struct =
        tpe

  def unionEnc[A](tpe: TsType.Union): UnionEncoder[A] =
    new UnionEncoder[A]:
      override def encode: TsType.Union =
        tpe
