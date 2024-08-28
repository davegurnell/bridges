package bridges.core

import shapeless._
import shapeless.labelled.FieldType

import scala.reflect.runtime.universe.WeakTypeTag

trait DerivedEncoderInstances extends DerivedEncoderInstances1 {
  implicit def valueClassEncoder[A <: AnyVal, B](implicit unwrapped: Unwrapped.Aux[A, B], encoder: BasicEncoder[B]): BasicEncoder[A] =
    pure(encoder.encode)
}

trait DerivedEncoderInstances1 extends DerivedEncoderInstances0 {
  import Type._

  implicit val hnilProdEncoder: ProdEncoder[HNil] =
    pureProd(Prod(Nil))

  implicit def hconsProdEncoder[K <: Symbol, H, T <: HList](implicit
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

  implicit def cconsSumEncoder[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      hEnc: Lazy[ProdEncoder[H]],
      tEnc: SumEncoder[T]
  ): SumEncoder[FieldType[K, H] :+: T] = {
    val name    = witness.value.name
    val product = hEnc.value.encode
    val tail    = tEnc.encode
    pureSum(Sum((name -> product) +: tail.products))
  }

  implicit def genericProdEncoder[A, R](implicit
      gen: LabelledGeneric.Aux[A, R],
      enc: Lazy[ProdEncoder[R]]
  ): ProdEncoder[A] =
    pureProd(enc.value.encode)

  implicit def genericSumEncoder[A, R](implicit
      gen: LabelledGeneric.Aux[A, R],
      enc: Lazy[SumEncoder[R]]
  ): SumEncoder[A] =
    pureSum(enc.value.encode)
}

trait DerivedEncoderInstances0 extends EncoderConstructors {
  import Type._

  implicit def genericBasicEncoder[A](implicit low: LowPriority, tpeTag: WeakTypeTag[A]): BasicEncoder[A] =
    pure(Ref(TypeName.getTypeName[A]))
}
