package bridges.core

final case class DeclF[+A <: Type](name: String, tpe: A) {
  def renameRef(from: String, to: String): DeclF[A] =
    DeclF(
      if (name == from) to else name,
      tpe.renameRef(from, to).asInstanceOf[A]
    )
}

sealed abstract class Type extends Product with Serializable {
  import Type._

  def renameRef(from: String, to: String): Type =
    this match {
      case Ref(`from`)  => Ref(to)
      case tpe: Ref     => tpe
      case tpe @ Str    => tpe
      case tpe @ Chr    => tpe
      case tpe @ Intr   => tpe
      case tpe @ Real   => tpe
      case tpe @ Bool   => tpe
      case Opt(tpe)     => Opt(tpe.renameRef(from, to))
      case Arr(tpe)     => Arr(tpe.renameRef(from, to))
      case Prod(fields) => Prod(fields.map(_.renameRef(from, to)))
      case Sum(types)   => Sum(types.map(_.renameRef(from, to)))
    }
}

object Type {
  final case class Ref(id: String) extends Type
  final case object Str            extends Type
  final case object Chr            extends Type
  final case object Intr           extends Type
  final case object Real           extends Type
  final case object Bool           extends Type
  final case class Opt(tpe: Type)  extends Type
  final case class Arr(tpe: Type)  extends Type

  final case class Prod(fields: List[Decl]) extends Type {
    def +:(field: Decl): Prod =
      Prod(field +: fields)

    def :+(field: Decl): Prod =
      Prod(fields :+ field)
  }

  final case class Sum(products: List[ProdDecl]) extends Type {
    def +:(prod: ProdDecl): Sum =
      Sum(prod +: products)

    def :+(prod: ProdDecl): Sum =
      Sum(products :+ prod)
  }
}
