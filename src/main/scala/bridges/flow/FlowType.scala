package bridges.flow

import bridges.core._
import bridges.flow.syntax._

sealed abstract class FlowType extends Product with Serializable {
  import FlowType._

  def |(that: FlowType): FlowType =
    Union(List(this, that))

  def &(that: FlowType): FlowType =
    Inter(List(this, that))

  def ? : FlowType =
    Opt(this)
}

object FlowType {
  final case class Ref(id: String, params: List[FlowType] = Nil) extends FlowType

  final case object Str       extends FlowType
  final case object Chr       extends FlowType
  final case object Intr      extends FlowType
  final case object Real      extends FlowType
  final case object Bool      extends FlowType
  final case object Null      extends FlowType
  final case object Undefined extends FlowType

  final case class StrLit(value: String)        extends FlowType
  final case class ChrLit(value: Char)          extends FlowType
  final case class IntrLit(value: Int)          extends FlowType
  final case class RealLit(value: Double)       extends FlowType
  final case class BoolLit(value: Boolean)      extends FlowType
  final case class Opt(tpe: FlowType)           extends FlowType
  final case class Arr(tpe: FlowType)           extends FlowType
  final case class Tuple(types: List[FlowType]) extends FlowType

  final case class Struct(fields: List[FlowField], rest: Option[FlowRestField] = None) extends FlowType {
    def withRest(keyType: FlowType, valueType: FlowType, keyName: String = "key"): Struct =
      copy(rest = Some(FlowRestField(keyName, keyType, valueType)))
  }
  final case class Inter(types: List[FlowType]) extends FlowType
  final case class Union(types: List[FlowType]) extends FlowType

  def from(tpe: Type)(implicit config: FlowEncoderConfig): FlowType =
    tpe match {
      case Type.Ref(id, params)  => Ref(id, params.map(from))
      case Type.Str              => Str
      case Type.Chr              => Chr
      case Type.Intr             => Intr
      case Type.Real             => Real
      case Type.Bool             => Bool
      case Type.Opt(tpe)         => Opt(from(tpe))
      case Type.Arr(tpe)         => Arr(from(tpe))
      case Type.Dict(kTpe, vTpe) => Struct(Nil, Some(FlowRestField("key", from(kTpe), from(vTpe))))
      case Type.Prod(fields)     => translateProd(fields)
      case Type.Sum(products)    => translateSum(products)
    }

  private def translateProd(fields: List[(String, Type)])(implicit config: FlowEncoderConfig): Struct =
    Struct(fields.map { case (name, tpe) => FlowField(name, from(tpe), keyIsOptional(tpe)) })

  private def translateSum(products: List[(String, Type.Prod)])(implicit config: FlowEncoderConfig): Union =
    Union(products.map {
      case (name, prod) =>
        Struct(FlowField("type", StrLit(name)) +: translateProd(prod.fields).fields)
    })

  private def keyIsOptional(tpe: Type)(implicit config: FlowEncoderConfig): Boolean =
    tpe match {
      case _: Type.Opt if config.optionalFields => true
      case _                                    => false
    }

  implicit val rename: Rename[FlowType] =
    Rename.pure { (value, from, to) =>
      def renameId(id: String): String =
        if (id == from) to else id

      value match {
        case Ref(id, params)      => Ref(renameId(id), params.map(_.rename(from, to)))
        case tpe @ Str            => tpe
        case tpe @ Chr            => tpe
        case tpe @ Intr           => tpe
        case tpe @ Real           => tpe
        case tpe @ Bool           => tpe
        case tpe @ Null           => tpe
        case tpe @ Undefined      => tpe
        case tpe: StrLit          => tpe
        case tpe: ChrLit          => tpe
        case tpe: IntrLit         => tpe
        case tpe: RealLit         => tpe
        case tpe: BoolLit         => tpe
        case Opt(tpe)             => Opt(tpe.rename(from, to))
        case Arr(tpe)             => Arr(tpe.rename(from, to))
        case Tuple(types)         => Tuple(types.map(_.rename(from, to)))
        case Struct(fields, rest) => Struct(fields.map(_.rename(from, to)), rest.map(_.rename(from, to)))
        case Inter(types)         => Inter(types.map(_.rename(from, to)))
        case Union(types)         => Union(types.map(_.rename(from, to)))
      }
    }
}
