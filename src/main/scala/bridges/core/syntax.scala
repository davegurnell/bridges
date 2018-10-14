package bridges.core

import shapeless.{ Lazy, Typeable }

object syntax extends RenamableSyntax {
  import Type._

  def encode[A: Encoder]: Type =
    Encoder[A].encode

  def typeName[A](implicit typeable: Typeable[A]): String =
    typeable.describe.takeWhile(c ⇒ c != '[' && c != '.').mkString

  def decl[A](implicit typeable: Typeable[A], encoder: Lazy[Encoder[A]]): Decl =
    DeclF(typeName[A], encoder.value.encode)

  def prod(fields: Decl*): Prod =
    Prod(fields.toList)

  def sum(products: ProdDecl*): Sum =
    Sum(products.toList)

  implicit class StringDeclOps(str: String) {
    def :=[A](tpe: A): DeclF[A] =
      DeclF(str, tpe)
  }
}
