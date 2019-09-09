package bridges.core

import bridges.core.syntax._

/** Representation of a nominal, sum-of-products style type.
  *
  * We can encode Scala ADTs to this representation:
  *
  *  - sealed traits become instances of Sum;
  *  - case classes become instances of Prod;
  *  - references to other types in the body of a Sum or Prod become Refs;
  *  - we have special encodings for Options and sequences
  * (which are normally handled specially in the target language).
  *
  * We can generate Elm bindings directly from this representation.
  * For Flow and Typescript bindings we
  * translate to other intermediate representations.
  */
sealed abstract class Type extends Product with Serializable

object Type {
  final case class Ref(id: String, params: List[Type] = Nil) extends Type
  final case object Str                                      extends Type
  final case object Chr                                      extends Type
  final case object Intr                                     extends Type
  final case object Real                                     extends Type
  final case object Bool                                     extends Type
  final case class Opt(tpe: Type)                            extends Type
  final case class Arr(tpe: Type)                            extends Type
  final case class Dict(keys: Type, values: Type)            extends Type
  final case class Prod(fields: List[(String, Type)])        extends Type
  final case class Sum(products: List[(String, Prod)])       extends Type

  implicit private val prodRename: Rename[Prod] =
    Rename.pure { (tpe, from, to) =>
      tpe match {
        case Prod(fields) => Prod(fields.map(_.rename(from, to)))
      }
    }

  implicit val rename: Rename[Type] =
    Rename.pure { (tpe, from, to) =>
      def renameId(id: String): String =
        if (id == from) to else id

      tpe match {
        case Ref(id, params)  => Ref(renameId(id), params.map(_.rename(from, to)))
        case tpe @ Str        => tpe
        case tpe @ Chr        => tpe
        case tpe @ Intr       => tpe
        case tpe @ Real       => tpe
        case tpe @ Bool       => tpe
        case Opt(tpe)         => Opt(tpe.rename(from, to))
        case Arr(tpe)         => Arr(tpe.rename(from, to))
        case Dict(kTpe, vTpe) => Dict(kTpe.rename(from, to), vTpe.rename(from, to))
        case Prod(fields)     => Prod(fields.map(_.rename(from, to)))
        case Sum(products)    => Sum(products.map(_.rename(from, to)))
      }
    }
}
