package bridges.typescript

import bridges.core.{ DeclF, Encoder, RenamableSyntax }
import bridges.core.syntax.getCleanTagName
import shapeless.Lazy
import scala.reflect.runtime.universe.WeakTypeTag

object syntax extends RenamableSyntax {
  import TsType._

  def encode[A](implicit encoder: TsEncoder[A]): TsType =
    encoder.encode

  def decl[A](implicit tpeTag: WeakTypeTag[A], encoder: Lazy[TsEncoder[A]]): TsDecl =
    DeclF(getCleanTagName[A], encoder.value.encode)

  def struct(fields: TsDecl*): Struct =
    Struct(fields.toList)

  def union(types: TsType*): Union =
    Union(types.toList)

  def inter(types: TsType*): Inter =
    Inter(types.toList)

  implicit class StringDeclOps(str: String) {
    def :=[A](tpe: A): DeclF[A] =
      DeclF(str, tpe)
  }
}
