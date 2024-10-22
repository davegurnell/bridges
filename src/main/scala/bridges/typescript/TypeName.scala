package bridges.typescript

import scala.compiletime.*
import scala.quoted.*

object TypeName:
  inline given of[A]: String =
    ${ nameImpl[A] }

  private def nameImpl[A: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    val typeRepr = TypeRepr.of[A]
    Expr(typeRepr.show.split("\\.").last)
