package bridges.typescript

import bridges.core.{ DeclF, Encoder, RenamableSyntax }
import shapeless.{ Lazy, Typeable }

object syntax extends RenamableSyntax {
  import TsType._

  def encode[A: Encoder]: TsType =
    from(Encoder[A].encode)

  def typeName[A](implicit typeable: Typeable[A]): String =
    typeable.describe.takeWhile(c ⇒ c != '[' && c != '.').mkString

  def decl[A](implicit typeable: Typeable[A], encoder: Lazy[Encoder[A]]): TsDecl =
    DeclF(typeName[A], encoder.value.encode).map(TsType.from)

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
