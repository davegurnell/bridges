package bridges.flow

import bridges.core._
import shapeless.Lazy

import scala.language.implicitConversions
import scala.reflect.runtime.universe.WeakTypeTag

object syntax extends RenamableSyntax {
  import FlowType._

  def encode[A](implicit encoder: FlowEncoder[A]): FlowType =
    encoder.encode

  def decl[A](implicit tpeTag: WeakTypeTag[A], encoder: Lazy[FlowEncoder[A]]): FlowDecl =
    DeclF(TypeName.getTypeName[A], encoder.value.encode)

  def decl(name: String, params: String*)(tpe: FlowType): FlowDecl =
    DeclF(name, params.toList, tpe)

  def struct(fields: FlowField*): Struct =
    Struct(fields.toList)

  implicit class StringFieldOps(name: String) {
    def -->(tpe: FlowType): FlowField =
      FlowField(name, tpe, optional = false)

    def -?>(tpe: FlowType): FlowField =
      FlowField(name, tpe, optional = true)
  }

  @deprecated("Use --> instead of ->", "0.16.0")
  implicit def pairToField(pair: (String, FlowType)): FlowField = {
    val (name, tpe) = pair
    FlowField(name, tpe, optional = false)
  }

  def field(name: String, optional: Boolean = false)(tpe: FlowType): FlowField =
    FlowField(name, tpe, optional)

  def restField(name: String, keyType: FlowType)(valueType: FlowType): FlowRestField =
    FlowRestField(name, keyType, valueType)

  def tuple(types: FlowType*): FlowType =
    Tuple(types.toList)

  def union(types: FlowType*): FlowType =
    Union(types.toList)

  def inter(types: FlowType*): FlowType =
    Inter(types.toList)

  def arr(tpe: FlowType): FlowType =
    Arr(tpe)

  def dict(keyType: FlowType, valueType: FlowType): FlowType =
    Struct(Nil, Some(FlowRestField("key", keyType, valueType)))

  def ref(name: String, params: FlowType*): Ref =
    Ref(name, params.toList)

  implicit class StringDeclOps(str: String) {
    def :=[A](tpe: A): DeclF[A] =
      DeclF(str, tpe)
  }
}
