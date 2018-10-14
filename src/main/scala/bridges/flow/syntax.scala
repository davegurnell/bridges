package bridges.flow

import bridges.core._
import shapeless.{ Lazy, Typeable }

object syntax extends RenamableSyntax {
  import FlowType._

  def encode[A: Encoder]: FlowType =
    from(Encoder[A].encode)

  def typeName[A](implicit typeable: Typeable[A]): String =
    typeable.describe.takeWhile(c ⇒ c != '[' && c != '.').mkString

  def decl[A](implicit typeable: Typeable[A], encoder: Lazy[Encoder[A]]): FlowDecl =
    DeclF(typeName[A], encoder.value.encode).map(FlowType.from)

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
