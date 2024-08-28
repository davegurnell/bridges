package bridges.core

import scala.reflect.runtime.universe.WeakTypeTag

object TypeName {
  // NOTE: we can't use `shapeless.Typeable` in here as it breaks the code for recursive types like
  //   final case class Recursive(head: Int, tail: Option[Recursive])
  //
  // The only solution I found is to use a `WeakTypeTag` from scala runtime,
  // which seems to manage the recursivity OK.
  def getTypeName[A](implicit tpeTag: WeakTypeTag[A]): String = {
    val fullName = tpeTag.tpe.typeSymbol.fullName
    fullName.split('.').last
  }
}
