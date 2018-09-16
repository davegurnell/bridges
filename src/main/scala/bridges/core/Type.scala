package bridges.core

sealed abstract class Type extends Product with Serializable {
  import Type._

  def renameRef(from: String, to: String): Type =
    this match {
      case Ref(`from`)     => Ref(to)
      case tpe: Ref        => tpe
      case tpe @ Str       => tpe
      case tpe @ Character => tpe
      case tpe @ Num       => tpe
      case tpe @ Floating  => tpe
      case tpe @ Bool      => tpe
      case Optional(tpe)   => Optional(tpe.renameRef(from, to))
      case Array(tpe)      => Array(tpe.renameRef(from, to))
      case Struct(fields)  => Struct(fields.renameRef(from, to))
      case tpe: AProduct   => renameProduct(from, to, tpe)
      case SumOfProducts(types) =>
        SumOfProducts(types.map(renameProduct(from, to, _)))
    }

  private def renameProduct(from: String, to: String, product: AProduct): AProduct = {
    val newName       = if (product.name == from) to else product.name
    val renamedFields = Struct(product.struct.fields.renameRef(from, to))
    AProduct(newName, renamedFields)
  }
}

object Type {
  final case class Ref(id: String) extends Type

  final case object Str                extends Type
  final case object Character          extends Type
  final case object Num                extends Type
  final case object Floating           extends Type
  final case object Bool               extends Type
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

  final case class AProduct(name: String, struct: Struct) extends Type

  final case class SumOfProducts(types: List[AProduct]) extends Type

  object SumOfProducts {
    def apply(products: AProduct*): SumOfProducts =
      SumOfProducts(products.toList)
  }

  /*
    - key : contains a Struct (key -> StrLiteral(name)) that identifies the type. Can probably be done better...
    - tpe: the type in this intersection
    - fields: a Struct that contains all the fields associated to the Type of the Intersection, so we can derive on these values
    //TODO: intersection must be a specific structure for Flow/Typescrypt, see expected output and act accordingly
   */
//  final case class Intersection(key: AProduct, tpe: Type, fields: AProduct) extends Type
//
//  def disc(name: String, tpe: Type, fields: AProduct): Intersection =
//    disc("type")(name, tpe, fields)
//
//  def disc(key: String)(name: String, tpe: Type, fields: AProduct): Intersection =
//    Intersection(AProduct(key -> StrLiteral(name)), tpe, fields)
//
//  def discUnion(types: (String, Type, AProduct)*): Union =
//    discUnion("type")(types: _*)
//
//  def discUnion(key: String)(types: (String, Type, AProduct)*): Union =
//    Union(types.map { case (name, tpe, fields) => disc(key)(name, tpe, fields) }.toList)

  implicit class TypeMapOps(types: List[(String, Type)]) {
    def renameRef(from: String, to: String): List[(String, Type)] =
      types.map {
        case (name, tpe) =>
          (name, tpe.renameRef(from, to))
      }
  }
}
