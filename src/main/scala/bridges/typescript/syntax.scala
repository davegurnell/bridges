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

  def decl(name: String, params: String*)(tpe: TsType): TsDecl =
    DeclF(name, params.toList, tpe)

  def struct(fields: (String, TsType)*): Struct =
    Struct(fields.toList)

  def union(types: TsType*): Union =
    Union(types.toList)

  def inter(types: TsType*): Inter =
    Inter(types.toList)

  def ref(name: String, params: TsType*): Ref =
    Ref(name, params.toList)

  implicit class StringDeclOps(str: String) {
    def :=[A](tpe: A): DeclF[A] =
      DeclF(str, tpe)
  }
}
