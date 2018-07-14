package bridges.core

import bridges.syntax.typeName
import scala.language.higherKinds
import shapeless._
import shapeless.labelled.FieldType

trait Encoder[A] {
  def encode: Type
}

trait StructEncoder[A] extends Encoder[A] {
  override def encode: Type.Struct
}

trait UnionEncoder[A] extends Encoder[A] {
  override def encode: Type.Union
}

trait BasicEncoder[A] extends Encoder[A]

object Encoder extends EncoderInstances2

trait EncoderInstances2 extends EncoderInstances1 {
  import Type._

  implicit val stringEncoder: BasicEncoder[String] =
    pure(Str)

  implicit val charEncoder: BasicEncoder[Char] =
    pure(Character)

  implicit val intEncoder: BasicEncoder[Int] =
    pure(Num)

  implicit val doubleEncoder: BasicEncoder[Double] =
    pure(Floating)

  implicit val floatEncoder: BasicEncoder[Float] =
    pure(Floating)

  implicit val booleanEncoder: BasicEncoder[Boolean] =
    pure(Bool)

  implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
    pure(UUIDType)

  implicit def optionEncoder[A](
      implicit enc: BasicEncoder[A]
  ): BasicEncoder[Option[A]] =
    pure(Optional(enc.encode))

  implicit def traversableEncoder[F[_] <: Traversable[_], A](
      implicit enc: BasicEncoder[A]
  ): BasicEncoder[F[A]] =
    pure(Array(enc.encode))

  implicit def valueClassEncoder[A <: AnyVal, B](
      implicit unwrapped: Unwrapped.Aux[A, B],
      encoder: BasicEncoder[B]
  ): BasicEncoder[A] =
    pure(encoder.encode)
}

trait EncoderInstances1 extends EncoderInstances0 {
  import Type._

  implicit val hnilEncoder: StructEncoder[HNil] =
    pureStruct(Struct(Nil))

  implicit def hconsEncoder[K <: Symbol, H, T <: HList](
      implicit
      witness: Witness.Aux[K],
      hEnc: Lazy[BasicEncoder[H]],
      tEnc: StructEncoder[T]
  ): StructEncoder[FieldType[K, H] :: T] = {
    val name = witness.value.name
    val head = hEnc.value.encode
    val tail = tEnc.encode
    pureStruct((name, head) +: tail)
  }

  implicit def cnilUnionEncoder: UnionEncoder[CNil] =
    pureUnion(Union(Nil))

  // we should always have a StructEncoder for H as on a Coproduct
  implicit def cconsUnionEncoder[K <: Symbol, H, T <: Coproduct](
      implicit
      witness: Witness.Aux[K],
      hEnc: Lazy[BasicEncoder[H]],
      hEncFields: Lazy[StructEncoder[H]],
      tEnc: UnionEncoder[T]
  ): UnionEncoder[FieldType[K, H] :+: T] = {
    val name = witness.value.name
    val head = hEnc.value.encode
    val headFields = hEncFields.value.encode
    val tail = tEnc.encode

    pureUnion(disc(name, head, headFields) +: tail)
  }

  implicit def genericStructEncoder[A, L](
      implicit gen: LabelledGeneric.Aux[A, L],
      enc: Lazy[StructEncoder[L]]
  ): StructEncoder[A] =
    pureStruct(enc.value.encode)

  implicit def genericUnionEncoder[A, L](
      implicit gen: LabelledGeneric.Aux[A, L],
      enc: Lazy[UnionEncoder[L]]
  ): UnionEncoder[A] =
    pureUnion(enc.value.encode)
}

trait EncoderInstances0 extends EncoderConstructors {
  import Type._

  implicit def genericBasicEncoder[A](
      implicit typeable: Typeable[A],
      low: LowPriority
  ): BasicEncoder[A] =
    pure(Ref(typeName[A]))
}

trait EncoderConstructors {
  import Type._

  def apply[A](implicit enc: Encoder[A]): Encoder[A] =
    enc

  def pure[A](tpe: Type): BasicEncoder[A] =
    new BasicEncoder[A] { def encode: Type = tpe }

  def pureStruct[A](tpe: Struct): StructEncoder[A] =
    new StructEncoder[A] { def encode: Struct = tpe }

  def pureUnion[A](tpe: Union): UnionEncoder[A] =
    new UnionEncoder[A] { def encode: Union = tpe }
}
