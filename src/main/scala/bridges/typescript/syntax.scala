package bridges.typescript

import bridges.core.{ DeclF, Encoder, RenamableSyntax }
import bridges.core.syntax.getCleanTagName
import shapeless.Lazy
import scala.reflect.runtime.universe.WeakTypeTag

object syntax extends RenamableSyntax {
  import TsType._

  def encode[A: Encoder]: TsType =
    from(Encoder[A].encode)

  def decl[A](implicit tpeTag: WeakTypeTag[A], encoder: Lazy[Encoder[A]]): TsDecl =
    DeclF(getCleanTagName[A], encoder.value.encode).map(TsType.from)

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
