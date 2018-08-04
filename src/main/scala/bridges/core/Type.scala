package bridges.core

sealed abstract class Type extends Product with Serializable {
  import Type._

  def unionTypes: List[Type] = this match {
    case Union(types) => types
    case _            => List(this)
  }

  def |(that: Type): Union =
    Union(this.unionTypes ++ that.unionTypes)

  def renameRef(from: String, to: String): Type =
    this match {
      case Ref(`from`)          => Ref(to)
      case tpe: Ref             => tpe
      case tpe: StrLiteral      => tpe
      case tpe: CharLiteral     => tpe
      case tpe: NumLiteral      => tpe
      case tpe: FloatingLiteral => tpe
      case tpe: BoolLiteral     => tpe
      case tpe: UUIDLiteral     => tpe
      case tpe: Str             => tpe
      case tpe: Character       => tpe
      case tpe: Num             => tpe
      case tpe: Floating        => tpe
      case tpe: Bool            => tpe
      case tpe: UUIDType        => tpe
      case Optional(tpe)        => Optional(tpe.renameRef(from, to))
      case Array(tpe)           => Array(tpe.renameRef(from, to))
      case Struct(fields)       => Struct(fields.renameRef(from, to))
      case Union(types)         => Union(types.map(_.renameRef(from, to)))

      case Intersection(Struct(keys), tpe, Struct(fields)) =>
        Intersection(
          Struct(keys.renameRef(from, to)),
          tpe.renameRef(from, to),
          Struct(fields.renameRef(from, to))
        )
    }
}

object Type {
  final case class Ref(id: String) extends Type

  sealed abstract class Str                  extends Type with Product with Serializable
  final case class StrLiteral(value: String) extends Str
  final case object Str                      extends Str

  sealed abstract class Character           extends Type with Product with Serializable
  final case class CharLiteral(value: Char) extends Character
  final case object Character               extends Character

  sealed abstract class Num               extends Type with Product with Serializable
  final case class NumLiteral(value: Num) extends Num
  final case object Num                   extends Num

  sealed abstract class Floating                    extends Type with Product with Serializable
  final case class FloatingLiteral(value: Floating) extends Floating
  final case object Floating                        extends Floating

  sealed abstract class Bool                   extends Type with Product with Serializable
  final case class BoolLiteral(value: Boolean) extends Bool
  final case object Bool                       extends Bool

  sealed abstract class UUIDType                      extends Type with Product with Serializable
  final case class UUIDLiteral(value: java.util.UUID) extends UUIDType
  final case object UUIDType                          extends UUIDType

  final case class Optional(tpe: Type) extends Type
  final case class Array(tpe: Type)    extends Type

  final case class Struct(fields: List[(String, Type)]) extends Type {
    def +:(field: (String, Type)): Struct =
      Struct(field +: fields)
  }

  object Struct {
    def apply(fields: (String, Type)*): Struct =
      Struct(fields.toList)
  }

  //TODO: does union only contain intersections?
  final case class Union(types: List[Type]) extends Type {
    def +:(tpe: Type): Union =
      Union(tpe +: types)
  }

  object Union {
    def apply(types: Type*): Union =
      Union(types.toList)
  }

  /*
    - key : contains a Struct (key -> StrLiteral(name)) that identifies the type. Can probably be done better...
    - tpe: the type in this intersection
    - fields: a Struct that contains all the fields associated to the Type of the Intersection, so we can derive on these values
   */
  final case class Intersection(key: Struct, tpe: Type, fields: Struct) extends Type

  def disc(name: String, tpe: Type, fields: Struct): Intersection =
    disc("type")(name, tpe, fields)

  def disc(key: String)(name: String, tpe: Type, fields: Struct): Intersection =
    Intersection(Struct(key -> StrLiteral(name)), tpe, fields)

  def discUnion(types: (String, Type, Struct)*): Union =
    discUnion("type")(types: _*)

  def discUnion(key: String)(types: (String, Type, Struct)*): Union =
    Union(types.map { case (name, tpe, fields) => disc(key)(name, tpe, fields) }.toList)

  implicit class TypeMapOps(types: List[(String, Type)]) {
    def renameRef(from: String, to: String): List[(String, Type)] =
      types.map {
        case (name, tpe) =>
          (name, tpe.renameRef(from, to))
      }
  }
}
