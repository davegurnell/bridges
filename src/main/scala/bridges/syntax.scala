package bridges

import shapeless.{Lazy, Typeable}

object syntax {
  import Type._

  def encode[A: Encoder]: Type =
    Encoder[A].encode

  def declaration[A](implicit typeable: Typeable[A], encoder: Lazy[Encoder[A]]): Declaration =
    Declaration(typeable.describe, encoder.value.encode)

  def render[A](decls: List[Declaration])(implicit renderer: Renderer[A]): String =
    renderer.render(decls)

  implicit class StringOps(str: String) {
    def := [A](tpe: Type): Declaration =
      Declaration(str, tpe)
  }

  implicit class StringPairOps(a: (String, Type)) {
    def |(b: (String, Type)): Union =
      discUnion(a, b)
  }
}
