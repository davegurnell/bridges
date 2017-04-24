package bridges.ts

sealed abstract class Type extends Product with Serializable {
  import Type._

  def toUnion: Union = this match {
    case _: Binding => Union(List(this))
    case _: Ref     => Union(List(this))
    case _: Str     => Union(List(this))
    case _: Num     => Union(List(this))
    case _: Bool    => Union(List(this))
    case Null       => Union(List(this))
    case _: Array   => Union(List(this))
    case _: Struct  => Union(List(this))
    case u: Union   => u
  }

  def |(that: Type): Union =
    Union(this.toUnion.types ++ that.toUnion.types)

  def flatten: Type =
    this match {
      case Binding(a, b)  => Ref(a)
      case _: Ref         => this
      case _: Str         => this
      case _: Num         => this
      case _: Bool        => this
      case Null           => this
      case Array(tpe)     => Array(tpe.flatten)
      case Struct(fields) => Struct(fields.map(b => Binding(b.id, b.`type`.flatten)))
      case Union(types)   => Union(types.map(_.flatten))
    }

  def bindings: List[Binding] = {
    def loop(curr: Type, accum: List[Binding]): List[Binding] =
      curr match {
        case Binding(a, b)  => loop(b, Binding(a, b.flatten) :: accum)
        case _: Ref         => accum
        case _: Str         => accum
        case _: Num         => accum
        case _: Bool        => accum
        case Null           => accum
        case Array(tpe)     => loop(tpe, accum)
        case Struct(fields) => fields.foldRight(accum)((curr, acc) => loop(curr.`type`, acc))
        case Union(types)   => types.foldRight(accum)((curr, acc) => loop(curr, acc))
      }

    loop(this, Nil).distinct
  }
}

object Type {
  final case class Binding(id: String, `type`: Type) extends Type

  final case class Ref(id: String) extends Type

  sealed abstract class Atom extends Type

  sealed abstract class Str extends Atom with Product with Serializable
  final case class StrLiteral(value: String) extends Str
  final case object Str extends Str

  sealed abstract class Num extends Atom with Product with Serializable
  final case class NumLiteral(value: Num) extends Num
  final case object Num extends Num

  sealed abstract class Bool extends Atom with Product with Serializable
  final case class BoolLiteral(value: Boolean) extends Bool
  final case object Bool extends Bool

  final case object Null extends Atom

  final case class Array(`type`: Type) extends Type

  final case class Union(types: List[Type]) extends Type {
    def +:(`type`: Type): Union =
      Union(`type` +: types)
  }

  final case class Struct(fields: List[Binding]) extends Type {
    def +:(field: Binding): Struct =
      Struct(field +: fields)
  }

  object Struct {
    def apply(bindings: (String, Type) *): Struct =
      Struct(bindings.map { case (k, v) => Binding(k, v) }.toList)
  }
}

sealed abstract class TsMeta extends Product with Serializable

object TsMeta {
  final case class Coproduct(name: String, subtypes: List[(String, TsMeta)]) extends TsMeta
  final case class Product(name: String, fields: List[(String, TsMeta)]) extends TsMeta
  final case class Atom(name: String, `type`: Type) extends TsMeta
}
