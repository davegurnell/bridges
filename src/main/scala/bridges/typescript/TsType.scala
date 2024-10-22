package bridges.typescript

enum TsType:
  import TsType._

  case Any
  case Unknown
  case Str
  case Chr
  case Intr
  case Real
  case Bool
  case Null

  case Ref(id: String, params: List[TsType] = Nil)
  case StrLit(value: String)
  case ChrLit(value: Char)
  case IntrLit(value: Int)
  case RealLit(value: Double)
  case BoolLit(value: Boolean)
  case Arr(tpe: TsType)
  case Tuple(types: List[TsType])
  case Func(args: List[(String, TsType)], ret: TsType)

  case Struct(fields: List[TsField], rest: Option[TsRestField] = None)

  case Inter(types: List[TsType])
  case Union(types: List[TsType])

  def isNullable: Boolean =
    this match
      case Null         => true
      case Union(types) => types.exists(_.isNullable)
      case _            => false

  def rename(from: String, to: String): TsType =
    def renameId(id: String): String =
      if (id == from) to else id

    this match
      case Ref(id, params)      => Ref(renameId(id), params.map(_.rename(from, to)))
      case Any                  => Any
      case tpe @ Unknown        => tpe
      case tpe @ Str            => tpe
      case tpe @ Chr            => tpe
      case tpe @ Intr           => tpe
      case tpe @ Real           => tpe
      case tpe @ Bool           => tpe
      case tpe @ Null           => tpe
      case tpe: StrLit          => tpe
      case tpe: ChrLit          => tpe
      case tpe: IntrLit         => tpe
      case tpe: RealLit         => tpe
      case tpe: BoolLit         => tpe
      case Arr(tpe)             => Arr(tpe.rename(from, to))
      case Tuple(types)         => Tuple(types.map(_.rename(from, to)))
      case Func(args, ret)      => Func(args.map((name, tpe) => (name, tpe.rename(from, to))), ret.rename(from, to))
      case Struct(fields, rest) => Struct(fields.map(_.rename(from, to)), rest.map(_.rename(from, to)))
      case Inter(types)         => Inter(types.map(_.rename(from, to)))
      case Union(types)         => Union(types.map(_.rename(from, to)))

object TsType:
  def ref(name: String, params: TsType*): Ref =
    Ref(name, params.toList)

  def tuple(types: TsType*): Tuple =
    Tuple(types.toList)

  def union(types: TsType*): Union =
    Union(types.toList)

  def intersect(types: TsType*): TsType =
    Inter(types.toList)

  def nullable(tpe: TsType): TsType =
    union(tpe, Null)

  def dict(keyType: TsType, valueType: TsType): Struct =
    Struct(Nil, Some(TsRestField("key", keyType, valueType)))

  def struct(fields: TsField*): Struct =
    Struct(fields.toList)

  def labelled(name: String, tpe: TsType): TsType =
    val discriminator = TsField("type", StrLit(name))
    tpe match
      case Struct(fields, rest) => Struct(discriminator :: fields, rest)
      case tpe                  => intersect(struct(discriminator), tpe)

  def discriminated(cases: (String, TsType)*): Union =
    union(cases.map(labelled)*)

  def func(args: (String, TsType)*)(ret: TsType): Func =
    Func(args.toList, ret)

extension (struct: TsType.Struct)
  def withRest(keyType: TsType, valueType: TsType, keyName: String = "key"): TsType.Struct =
    struct.copy(rest = Some(TsRestField(keyName, keyType, valueType)))
