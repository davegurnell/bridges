package bridges.core

import scala.quoted.*

object TypeName {
  inline def getTypeName[T]: String =
    ${ getTypeNameImpl[T] }

  private def getTypeNameImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect.*
    val typeRepr = TypeRepr.of[T]
    Expr(typeRepr.show.split("\\.").last)
  }
}
