package bridges.typescript

import bridges.core.{ DeclF, RenamableSyntax, TypeName }

object syntax extends RenamableSyntax {
  import TsType._

  def encode[A](implicit encoder: TsEncoder[A]): TsType =
    encoder.encode

  inline def decl[A](implicit encoder: => TsEncoder[A]): TsDecl =
    DeclF(TypeName.getTypeName[A], encoder.encode)

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
