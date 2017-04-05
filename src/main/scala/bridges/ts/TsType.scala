package bridges.ts

sealed abstract class TsType extends Product with Serializable {
  import TsType._

  def toUnion: Union = this match {
    case Str        => Union(List(this))
    case Num        => Union(List(this))
    case Bool       => Union(List(this))
    case Null       => Union(List(this))
    case _: Ref     => Union(List(this))
    case _: Binding => Union(List(this))
    case _: Struct  => Union(List(this))
    case u: Union   => u
  }

  def |(that: TsType): Union =
    Union(this.toUnion.types ++ that.toUnion.types)

  def flatten: TsType =
    this match {
      case Str            => this
      case Num            => this
      case Bool           => this
      case Null           => this
      case _: Ref         => this
      case Binding(a, b)  => Ref(a)
      case Struct(fields) => Struct(fields.map(b => Binding(b.id, b.`type`.flatten)))
      case Union(types)   => Union(types.map(_.flatten))
    }

  def bindings: List[Binding] = {
    def loop(curr: TsType, accum: List[Binding]): List[Binding] =
      curr match {
        case Str            => accum
        case Num            => accum
        case Bool           => accum
        case Null           => accum
        case _: Ref         => accum
        case Binding(a, b)  => loop(b, Binding(a, b.flatten) :: accum)

        case Struct(fields) =>
          fields.foldRight(accum) { (curr, accum) =>
            loop(curr.`type`, accum)
          }

        case Union(types) =>
          types.foldRight(accum) { (curr, accum) =>
            loop(curr, accum)
          }
      }

    loop(this, Nil).distinct
  }
}

object TsType {
  sealed abstract class Atom extends TsType
  final case object Str  extends Atom
  final case object Num  extends Atom
  final case object Bool extends Atom
  final case object Null extends Atom

  final case class Ref(id: String) extends TsType

  final case class Binding(id: String, `type`: TsType) extends TsType

  final case class Union(types: List[TsType]) extends TsType {
    def +:(`type`: TsType): Union =
      Union(`type` +: types)
  }

  final case class Struct(fields: List[Binding]) extends TsType {
    def +:(field: Binding): Struct =
      Struct(field +: fields)
  }

  object Struct {
    def apply(bindings: (String, TsType) *): Struct =
      Struct(bindings.map { case (k, v) => Binding(k, v) }.toList)
  }
}

sealed abstract class TsMeta extends Product with Serializable

object TsMeta {
  final case class Coproduct(name: String, subtypes: List[(String, TsMeta)]) extends TsMeta
  final case class Product(name: String, fields: List[(String, TsMeta)]) extends TsMeta
  final case class Atom(name: String, `type`: TsType) extends TsMeta
}
