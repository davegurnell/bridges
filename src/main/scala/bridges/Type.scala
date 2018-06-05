package bridges

sealed abstract class Type extends Product with Serializable {
  import Type._

  def unionTypes: List[Type] = this match {
    case Union(types) => types
    case _            => List(this)
  }

  def |(that: Type): Union =
    Union(this.unionTypes ++ that.unionTypes)
}

object Type {
  final case class Ref(id: String) extends Type

  sealed abstract class Str extends Type with Product with Serializable
  final case class StrLiteral(value: String) extends Str
  final case object Str extends Str

  sealed abstract class Character extends Type with Product with Serializable
  final case class CharLiteral(value: Char) extends Character
  final case object Character extends Character

  sealed abstract class Num extends Type with Product with Serializable
  final case class NumLiteral(value: Num) extends Num
  final case object Num extends Num

  sealed abstract class Floating extends Type with Product with Serializable
  final case class FloatingLiteral(value: Floating) extends Floating
  final case object Floating extends Floating

  sealed abstract class Bool extends Type with Product with Serializable
  final case class BoolLiteral(value: Boolean) extends Bool
  final case object Bool extends Bool

  final case class Optional(tpe: Type) extends Type
  final case class Array(tpe: Type) extends Type

  final case class Struct(fields: List[(String, Type)]) extends Type {
    def +:(field: (String, Type)): Struct =
      Struct(field +: fields)
  }

  object Struct {
    def apply(fields: (String, Type) *): Struct =
      Struct(fields.toList)
  }

  final case class Union(types: List[Type]) extends Type {
    def +:(tpe: Type): Union =
      Union(tpe +: types)
  }

  object Union {
    def apply(types: Type *): Union =
      Union(types.toList)
  }

  final case class Intersection(types: List[Type]) extends Type {
    def +:(tpe: Type): Intersection =
      Intersection(tpe +: types)
  }

  object Intersection {
    def apply(types: Type *): Intersection =
      Intersection(types.toList)
  }

  def disc(name: String, tpe: Type): Intersection =
    disc("type")(name, tpe)

  def disc(key: String)(name: String, tpe: Type): Intersection =
    Intersection(Struct(key -> StrLiteral(name)), tpe)

  def discUnion(types: (String, Type) *): Union =
    discUnion("type")(types : _*)

  def discUnion(key: String)(types: (String, Type) *): Union =
    Union(types.map { case (name, tpe) => disc(key)(name, tpe) }.toList)
}
