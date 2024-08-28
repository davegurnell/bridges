package bridges.typescript

import bridges.core.{ DeclF, RenamableSyntax, TypeName }
import shapeless.Lazy

import scala.language.implicitConversions
import scala.reflect.runtime.universe.WeakTypeTag

object syntax extends RenamableSyntax {
  import TsType._

  def encode[A](implicit encoder: TsEncoder[A]): TsType =
    encoder.encode

  def decl[A](implicit tpeTag: WeakTypeTag[A], encoder: Lazy[TsEncoder[A]]): TsDecl =
    DeclF(TypeName.getTypeName[A], encoder.value.encode)

  def decl(name: String, params: String*)(tpe: TsType): TsDecl =
    DeclF(name, params.toList, tpe)

  def struct(fields: TsField*): Struct =
    Struct(fields.toList)

  def dict(keyType: TsType, valueType: TsType): Struct =
    Struct(Nil, Some(TsRestField("key", keyType, valueType)))

  implicit class StringFieldOps(name: String) {
    def -->(tpe: TsType): TsField =
      TsField(name, tpe, optional = false)

    def -?>(tpe: TsType): TsField =
      TsField(name, tpe, optional = true)
  }

  @deprecated("Use --> instead of ->", "0.16.0")
  implicit def pairToField(pair: (String, TsType)): TsField = {
    val (name, tpe) = pair
    TsField(name, tpe, optional = false)
  }

  def field(name: String, optional: Boolean = false)(tpe: TsType): TsField =
    TsField(name, tpe, optional)

  def restField(name: String, keyType: TsType)(valueType: TsType): TsRestField =
    TsRestField(name, keyType, valueType)

  def tuple(types: TsType*): Tuple =
    Tuple(types.toList)

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

  def func(args: (String, TsType)*)(ret: TsType): Func =
    Func(args.toList, ret)
}
