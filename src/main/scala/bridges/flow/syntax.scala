package bridges.flow

import bridges.core._
import bridges.core.syntax.getCleanTagName
import shapeless.Lazy
import scala.reflect.runtime.universe.WeakTypeTag

object syntax extends RenamableSyntax {
  import FlowType._

  def encode[A](implicit encoder: FlowEncoder[A]): FlowType =
    encoder.encode

  def decl[A](implicit tpeTag: WeakTypeTag[A], encoder: Lazy[FlowEncoder[A]]): FlowDecl =
    DeclF(getCleanTagName[A], encoder.value.encode)

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
