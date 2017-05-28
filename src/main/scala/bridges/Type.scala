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

  sealed abstract class Num extends Type with Product with Serializable
  final case class NumLiteral(value: Num) extends Num
  final case object Num extends Num

  sealed abstract class Bool extends Type with Product with Serializable
  final case class BoolLiteral(value: Boolean) extends Bool
  final case object Bool extends Bool

  final case object Null extends Type

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

  final case class DiscUnion(types: List[(String, Type)]) extends Type {
    def +:(pair: (String, Type)): DiscUnion =
      DiscUnion(pair +: types)
  }

  object DiscUnion {
    def apply(types: (String, Type) *): DiscUnion =
      DiscUnion(types.toList)
  }
}
