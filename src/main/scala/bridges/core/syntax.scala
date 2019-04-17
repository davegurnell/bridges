package bridges.core

import shapeless.Lazy
import scala.reflect.runtime.universe.WeakTypeTag

object syntax extends RenamableSyntax {
  import Type._

  // NOTE: we can't use `shapeless.Typeable` in here as it breaks the code for recursive types like
  //   final case class Recursive(head: Int, tail: Option[Recursive])
  //
  // The only solution I found is to use a `WeakTypeTag` from scala runtime, which seems to
  // manage the recursivity OK. We just need some 'hacky' method to clean the output
  def getCleanTagName[A](implicit tpeTag: WeakTypeTag[A]): String = {
    // dirtyName will have form of: TypeTag[bridges.SampleTypes.Recurring]
    val dirtyName = tpeTag.toString()
    dirtyName.split('.').last.dropRight(1)
  }

  def encode[A: Encoder]: Type =
    Encoder[A].encode

  def decl[A](implicit tpeTag: WeakTypeTag[A], encoder: Lazy[Encoder[A]]): Decl =
    DeclF(getCleanTagName[A], encoder.value.encode)

  // To be used for classes with generic parameters as we can't use shapeless to derive them
  def decl(name: String, params: String*)(tpe: Type): Decl =
    DeclF(name, params.toList, tpe)

  def prod(fields: (String, Type)*): Prod =
    Prod(fields.toList)

  def sum(products: (String, Prod)*): Sum =
    Sum(products.toList)

  implicit class StringDeclOps(str: String) {
    def :=[A](tpe: A): DeclF[A] =
      DeclF(str, tpe)
  }
}
