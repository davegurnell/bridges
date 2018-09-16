package bridges

import bridges.core._
import shapeless.{ Lazy, Typeable }

object syntax {

  def encode[A: Encoder]: Type =
    Encoder[A].encode

  // latest Shapeless version adds typeable information of members to case classes, as well as to objects, which we don't want. Filtering that out while I discover a better fix
  def typeName[A](implicit typeable: Typeable[A]): String =
    typeable.describe.takeWhile(c ⇒ c != '[' && c != '.').mkString

  def declaration[A](implicit typeable: Typeable[A], encoder: Lazy[Encoder[A]]): Declaration =
    Declaration(typeName[A], encoder.value.encode)

  implicit class StringOps(str: String) {
    def :=[A](tpe: Type): Declaration =
      Declaration(str, tpe)
  }
//TODO is declaration needed really? or can we just override AProduct?
  implicit class DeclarationListOps(declarations: List[Declaration]) {
    def renameRef(from: String, to: String): List[Declaration] =
      declarations.map(_.renameRef(from, to))
  }
}
