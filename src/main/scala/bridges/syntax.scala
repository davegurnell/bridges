package bridges

import shapeless.Typeable

object syntax {
  import Type._

  def encode[A: Encoder]: Type =
    Encoder[A].encode

  def declaration[A](implicit typeable: Typeable[A], encoder: Encoder[A]): Declaration =
    Declaration(typeable.describe, encoder.encode)

  def render[A](decls: List[Declaration])(implicit renderer: Renderer[A]): String =
    renderer.render(decls)

  implicit class StringOps(str: String) {
    def :=(tpe: Type): Declaration =
      Declaration(str, tpe)
  }

  implicit class StringPairOps(a: (String, Type)) {
    def |(b: (String, Type)): DiscUnion =
      DiscUnion(List(a, b))
  }
}
