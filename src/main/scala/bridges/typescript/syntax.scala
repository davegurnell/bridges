package bridges.typescript

import bridges.core.{ DeclF, Encoder, RenamableSyntax }
// import bridges.core.syntax.getCleanTagName
// import shapeless.Lazy
import scala.compiletime.constValue
import scala.deriving.Mirror

object syntax extends RenamableSyntax:
  import TsType._

  def encode[A](using encoder: TsEncoder[A]): TsType =
    encoder.encode

  inline def decl[A](using mirror: Mirror.Of[A], encoder: => TsEncoder[A]): TsDecl =
    DeclF(constValue[mirror.MirroredLabel], encoder.encode)

  def decl(name: String, params: String*)(tpe: TsType): TsDecl =
    DeclF(name, params.toList, tpe)

  def struct(fields: TsField*): Struct =
    Struct(fields.toList)

  def dict(keyType: TsType, valueType: TsType): Struct =
    Struct(Nil, Some(TsRestField("key", keyType, valueType)))

  extension (name: String) def -->(tpe: TsType): TsField =
    TsField(name, tpe, optional = false)

  extension (name: String) def -?>(tpe: TsType): TsField =
    TsField(name, tpe, optional = true)

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

  def func(args: (String, TsType)*)(ret: TsType): Func =
    Func(args.toList, ret)
