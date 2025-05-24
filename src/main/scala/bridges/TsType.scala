package bridges

import bridges.syntax.RenamableOps

sealed abstract class TsType extends Product, Serializable {
  def |(that: TsType): TsType =
    TsType.Union(List(this, that)).flatten

  def &(that: TsType): TsType =
    TsType.Inter(List(this, that)).flatten

  def isNullable: Boolean =
    this match {
      case _: TsType.Ref       => false
      case TsType.Any          => false
      case TsType.Unknown      => false
      case TsType.Str          => false
      case TsType.Num          => false
      case TsType.Bool         => false
      case TsType.Null         => true
      case _: TsType.StrLit    => false
      case _: TsType.NumLit    => false
      case _: TsType.BoolLit   => false
      case _: TsType.Arr       => false
      case _: TsType.Tuple     => false
      case _: TsType.Struct    => false
      case _: TsType.Record    => false
      case _: TsType.Inter     => false
      case TsType.Union(types) => types.exists(_.isNullable)
      case _: TsType.Func      => false
    }
}

object TsType {
  final case class Ref(id: String, params: List[TsType] = Nil) extends TsType

  case object Any     extends TsType
  case object Unknown extends TsType
  case object Str     extends TsType
  case object Num     extends TsType
  case object Bool    extends TsType
  case object Null    extends TsType

  final case class StrLit(value: String)      extends TsType
  final case class NumLit(value: Double)      extends TsType
  final case class BoolLit(value: Boolean)    extends TsType
  final case class Arr(tpe: TsType)           extends TsType
  final case class Tuple(types: List[TsType]) extends TsType

  final case class Struct(fields: List[TsField], rest: Option[TsRestField] = None) extends TsType {
    def withRest(keyType: TsType, valueType: TsType, keyName: String = "key"): Struct =
      copy(rest = Some(TsRestField(keyName, keyType, valueType)))
  }

  final case class Record(keys: TsType, values: TsType) extends TsType

  final case class Inter(types: List[TsType]) extends TsType {
    def flatten: Inter =
      Inter(types.flatMap {
        case Inter(types) => types
        case tpe          => List(tpe)
      })
  }

  final case class Union(types: List[TsType]) extends TsType {
    def flatten: Union =
      Union(types.flatMap {
        case Union(types) => types
        case tpe          => List(tpe)
      })
  }

  final case class Func(args: List[(String, TsType)], ret: TsType) extends TsType

  implicit lazy val structRename: Rename[TsType.Struct] =
    Rename.instance { (struct, from, to) =>
      val TsType.Struct(fields, rest) = struct
      TsType.Struct(fields.map(Rename.rename(_, from, to)), rest.map(Rename.rename(_, from, to)))
    }

  implicit lazy val rename: Rename[TsType] =
    Rename.instance { (value, from, to) =>
      def renameId(id: String): String =
        if (id == from) to else id

      value match {
        case TsType.Ref(id, params)    => TsType.Ref(renameId(id), params.map(Rename.rename(_, from, to)))
        case TsType.Any                => TsType.Any
        case tpe @ TsType.Unknown      => tpe
        case tpe @ TsType.Str          => tpe
        case tpe @ TsType.Num          => tpe
        case tpe @ TsType.Bool         => tpe
        case tpe @ TsType.Null         => tpe
        case tpe: TsType.StrLit        => tpe
        case tpe: TsType.NumLit        => tpe
        case tpe: TsType.BoolLit       => tpe
        case TsType.Arr(tpe)           => TsType.Arr(tpe.rename(from, to))
        case TsType.Tuple(types)       => TsType.Tuple(types.map(Rename.rename(_, from, to)))
        case TsType.Func(args, ret)    => TsType.Func(args.map(Rename.rename(_, from, to)), ret.rename(from, to))
        case tpe: TsType.Struct        => tpe.rename(from, to)
        case TsType.Record(key, value) => TsType.Record(key.rename(from, to), value.rename(from, to))
        case TsType.Inter(types)       => TsType.Inter(types.map(Rename.rename(_, from, to)))
        case TsType.Union(types)       => TsType.Union(types.map(Rename.rename(_, from, to)))
      }
    }

  def discriminated(name: String): TsType =
    discriminated(name, Ref(name))

  def discriminated(name: String, tpe: TsType): TsType =
    Inter(List(
      Struct(List(TsField("type", StrLit(name)))),
      tpe
    ))
}
