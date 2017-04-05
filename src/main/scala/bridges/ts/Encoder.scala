package bridges.ts

import shapeless._
import shapeless.labelled._

trait Encoder[A] {
  def encode: TsType
}

trait StructEncoder[A] extends Encoder[A] {
  def encode: TsType.Struct
}

trait UnionEncoder[A] extends Encoder[A] {
  def encode: TsType.Union
}

trait LowPriorityEncoderImplicits {
  import TsType._

  def apply[A](implicit encoder: Encoder[A]): Encoder[A] =
    encoder

  def pure[A](`type`: TsType): Encoder[A] =
    new Encoder[A] { def encode = `type` }

  def pureStruct[A](`type`: Struct): StructEncoder[A] =
    new StructEncoder[A] { def encode = `type` }

  def pureUnion[A](`type`: Union): UnionEncoder[A] =
    new UnionEncoder[A] { def encode = `type` }

  implicit val hnilEncoder: StructEncoder[HNil] =
    pureStruct(Struct(Nil))

  implicit def hconsEncoder[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hEnc: Lazy[Encoder[H]],
    tEnc: StructEncoder[T]
  ): StructEncoder[FieldType[K, H] :: T] = {
    val name = witness.value.name
    val head = hEnc.value.encode
    val tail = tEnc.encode
    pureStruct(Binding(name, head) +: tail)
  }

  implicit def cnilEncoder: UnionEncoder[CNil] =
    pureUnion(Union(Nil))

  // We keep track of K in the result type here
  // to make this definition compatible with
  // the implicit parameters on genericEncoder.
  //
  // However, genericEncoder already encodes
  // the type name for each subtype in the coproduct
  // so we don't actually use K in the defininition here.
  implicit def cconsEncoder[K <: Symbol, H, T <: Coproduct](
    implicit
    hEnc: Lazy[Encoder[H]],
    tEnc: UnionEncoder[T]
  ): UnionEncoder[FieldType[K, H] :+: T] = {
    val head = hEnc.value.encode
    val tail = tEnc.encode
    pureUnion(head +: tail)
  }

  implicit def genericEncoder[A, L](
    implicit
    typeable: Typeable[A],
    gen: LabelledGeneric.Aux[A, L],
    enc: Lazy[Encoder[L]]
  ): Encoder[A] =
    pure(Binding(typeable.describe, enc.value.encode))
}

object Encoder extends LowPriorityEncoderImplicits {
  import TsType._

  implicit val stringEncoder: Encoder[String] =
    pure(Str)

  implicit val intEncoder: Encoder[Int] =
    pure(Num)

  implicit val doubleEncoder: Encoder[Double] =
    pure(Num)

  implicit val booleanEncoder: Encoder[Boolean] =
    pure(Bool)

  implicit def optionEncoder[A](implicit enc: Encoder[A]): Encoder[Option[A]] =
    pure(enc.encode | Null)

  implicit def valueClassEncoder[A <: AnyVal, B](
    implicit
    typeable: Typeable[A],
    unwrapped: Unwrapped.Aux[A, B],
    encoder: Encoder[B]
  ): Encoder[A] =
    pure(Binding(typeable.describe, encoder.encode))
}



// String

// // => string

// case class Foo(bar: String)

// // => type Foo = {
// //      bar: string,
// //    }

// sealed trait Foo
// case class Bar(...) extends Foo
// case class Baz(...) extends Foo

// // => type Bar = ...
// //    type Baz = ...
// //    type Foo = Bar | Baz
