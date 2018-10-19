package bridges.typescript

import bridges.core.{ DeclF, Encoder, RenamableSyntax }
import shapeless.{ Lazy, Typeable }

object syntax extends RenamableSyntax {
  import TsType._

  def encode[A](implicit encoder: TsEncoder[A]): TsType =
    encoder.encode

  def typeName[A](implicit typeable: Typeable[A]): String =
    typeable.describe.takeWhile(c ⇒ c != '[' && c != '.').mkString

  def decl[A](implicit typeable: Typeable[A], encoder: Lazy[TsEncoder[A]]): TsDecl =
    DeclF(typeName[A], encoder.value.encode)

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
