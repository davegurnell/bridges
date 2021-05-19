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
enum Type:
  case Ref(id: String, params: List[Type] = Nil)
  case Str
  case Chr
  case Intr
  case Real
  case Bool
  case Opt(tpe: Type)
  case Arr(tpe: Type)
  case Dict(keys: Type, values: Type)
  case Prod(fields: List[(String, Type)])
  case Sum(products: List[(String, Prod)])

object Type:
  given Rename[Prod] =
    Rename.pure { (tpe, from, to) =>
      tpe match
        case Prod(fields) => Prod(fields.map(_.rename(from, to)))
    }

  given Rename[Type] =
    Rename.pure { (tpe, from, to) =>
      def renameId(id: String): String =
        if id == from then to else id

      tpe match
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
