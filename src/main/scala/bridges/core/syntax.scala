package bridges.core

import shapeless.{ Lazy, Typeable }

object syntax {
  import Type._

  def encode[A: Encoder]: Type =
    Encoder[A].encode

  // latest Shapeless version adds typeable information of members to case classes, as well as to objects, which we don't want. Filtering that out while I discover a better fix
  def typeName[A](implicit typeable: Typeable[A]): String =
    typeable.describe.takeWhile(c ⇒ c != '[' && c != '.').mkString

  def decl[A](implicit typeable: Typeable[A], encoder: Lazy[Encoder[A]]): Decl =
    DeclF(typeName[A], encoder.value.encode)

  def ref(name: String): Ref =
    Ref(name)

  def prod(fields: Decl*): Prod =
    Prod(fields.toList)

  def sum(products: ProdDecl*): Sum =
    Sum(products.toList)

  implicit class StringDeclOps(str: String) {
    def :=[A <: Type](tpe: A): DeclF[A] =
      DeclF(str, tpe)
  }
}
