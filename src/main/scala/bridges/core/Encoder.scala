package bridges.core

import bridges.syntax.typeName
import eu.timepit.refined.api._
import scala.language.higherKinds
import shapeless._
import shapeless.labelled.FieldType

trait Encoder[A] {
  def encode: Type
}

trait StructEncoder[A] extends Encoder[A] {
  override def encode: Type.Struct
}

trait AProductEncoder[A] extends Encoder[A] {
  override def encode: Type.AProduct
}

trait SumOfProductsEncoder[A] extends Encoder[A] {
  override def encode: Type.SumOfProducts
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

  implicit def refinedEncoder[A, B](implicit enc: BasicEncoder[A]): BasicEncoder[Refined[A, B]] =
    Encoder.pure(enc.encode)

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

  implicit def cnilUnionEncoder: SumOfProductsEncoder[CNil] =
    pureSumOfProducts(Nil)

  // we should always have a ProductEncoder for H as on a Coproduct
  implicit def cconsSumOfProductsEncoder[K <: Symbol, H, T <: Coproduct](
      implicit
      hEnc: Lazy[AProductEncoder[H]],
      tEnc: SumOfProductsEncoder[T]
  ): SumOfProductsEncoder[FieldType[K, H] :+: T] = {
    val product = hEnc.value.encode
    val tail    = tEnc.encode
    pureSumOfProducts(product :: tail.types)
  }

  implicit def productsEncoder[A, L](
      implicit gen: LabelledGeneric.Aux[A, L],
      typeable: Typeable[A],
      enc: Lazy[StructEncoder[L]]
  ): AProductEncoder[A] = {
    val name   = typeName[A]
    val fields = enc.value.encode

    pureAProduct(AProduct(name, fields))
  }

  implicit def genericSumOfProductsEncoder[A, L <: Coproduct](
      implicit gen: LabelledGeneric.Aux[A, L],
      enc: Lazy[SumOfProductsEncoder[L]]
  ): SumOfProductsEncoder[A] =
    pureSumOfProducts(enc.value.encode)
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

  def pureAProduct[A](tpe: AProduct): AProductEncoder[A] =
    new AProductEncoder[A] { def encode: AProduct = tpe }

  def pureSumOfProducts[A](tpe: SumOfProducts): SumOfProductsEncoder[A] =
    new SumOfProductsEncoder[A] { def encode: SumOfProducts = tpe }

  def pureSumOfProducts[A](types: List[AProduct]): SumOfProductsEncoder[A] =
    new SumOfProductsEncoder[A] { def encode: SumOfProducts = SumOfProducts(types) }
}
