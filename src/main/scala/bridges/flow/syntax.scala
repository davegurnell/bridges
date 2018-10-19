package bridges.flow

import bridges.core._
import shapeless.{ Lazy, Typeable }

object syntax extends RenamableSyntax {
  import FlowType._

  def encode[A](implicit encoder: FlowEncoder[A]): FlowType =
    encoder.encode

  def typeName[A](implicit typeable: Typeable[A]): String =
    typeable.describe.takeWhile(c ⇒ c != '[' && c != '.').mkString

  def decl[A](implicit typeable: Typeable[A], encoder: Lazy[FlowEncoder[A]]): FlowDecl =
    DeclF(typeName[A], encoder.value.encode)

  def struct(fields: FlowDecl*): FlowType =
    Struct(fields.toList)

  def union(types: FlowType*): FlowType =
    Union(types.toList)

  def inter(types: FlowType*): FlowType =
    Inter(types.toList)

  implicit class StringDeclOps(str: String) {
    def :=[A](tpe: A): DeclF[A] =
      DeclF(str, tpe)
  }
}
