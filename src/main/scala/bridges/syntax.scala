package bridges

import scala.language.implicitConversions

object syntax extends RenamableSyntax {
  import TsType._

  def encode[A](implicit encoder: TopEncoder[A]): TsType =
    encoder.encode

  inline def decl[A](implicit encoder: TopEncoder[A]): TsDecl =
    TsDecl(TagName.getCleanTagName[A], encoder.encode)

  def decl(name: String, params: String*)(tpe: TsType): TsDecl =
    TsDecl(name, params.toList, tpe)

  def struct(fields: TsField*): Struct =
    Struct(fields.toList)

  def record(keyType: TsType, valueType: TsType): Record =
    Record(keyType, valueType)

  implicit class StringFieldOps(name: String) {
    def --->(tpe: TsType): TsField =
      TsField(name, tpe, optional = false)

    def --?>(tpe: TsType): TsField =
      TsField(name, tpe, optional = true)

    def -??>(tpe: TsType): TsField =
      TsField(name, tpe | Null, optional = true)
  }

  @deprecated("Use --> instead of ->", "0.16.0")
  implicit def pairToField(pair: (String, TsType)): TsField = {
    val (name, tpe) = pair
    TsField(name, tpe, optional = false)
  }

  def array(tpe: TsType): TsType =
    Arr(tpe)

  def field(name: String, optional: Boolean = false)(tpe: TsType): TsField =
    TsField(name, tpe, optional)

  def restField(name: String, keyType: TsType)(valueType: TsType): TsRestField =
    TsRestField(name, keyType, valueType)

  def tuple(types: TsType*): Tuple =
    Tuple(types.toList)

  def labelled(label: String)(tpe: TsType): TsType =
    tpe match {
      case Struct(fields, rest) => Struct(TsField("type", StrLit(label)) :: fields, rest)
      case tpe                  => Inter(List(Struct(List(TsField("type", StrLit(label)))), tpe))
    }

  def union(cases: TsType*): Union =
    Union(cases.toList)

  def inter(types: TsType*): Inter =
    Inter(types.toList)

  def nullable(tpe: TsType): TsType =
    tpe | Null

  def ref(name: String, params: TsType*): Ref =
    Ref(name, params.toList)

  def func(args: (String, TsType)*)(ret: TsType): Func =
    Func(args.toList, ret)
}
