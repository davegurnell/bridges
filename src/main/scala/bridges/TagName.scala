package bridges

import scala.quoted.*

object TagName {
  // NOTE: we can't use `shapeless.Typeable` in here as it breaks the code for recursive types like
  //   final case class Recursive(head: Int, tail: Option[Recursive])
  //
  // The only solution I found is to use a `WeakTypeTag` from scala runtime,
  // which seems to manage the recursivity OK.
  inline def getCleanTagName[A]: String =
    ${ getCleanTagNameImpl[A] }

  def getCleanTagNameImpl[A: Type](using Quotes): Expr[String] = {
    import quotes.reflect.*

    // Get the TypeRepr of T
    val typeRepr = TypeRepr.of[A]

    // Get the symbol of the type and retrieve its name
    val typeName = typeRepr.typeSymbol.name.filterNot(_ == '$')

    // Return the name as a string expression
    Expr(typeName)
  }
}
