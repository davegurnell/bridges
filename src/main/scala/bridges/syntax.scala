package bridges

import shapeless.{Lazy, Typeable}

object syntax {
  import Type._

  def encode[A: Encoder]: Type =
    Encoder[A].encode

  // TODO latest Shapeless version adds typeable information of members to case classes, which we don't want. Filtering that out while I discover a better fix
  def typeName[A](implicit typeable: Typeable[A]): String =
    typeable.describe.takeWhile(_ != '[').mkString

  def declaration[A](
      implicit typeable: Typeable[A],
      encoder: Lazy[Encoder[A]]
  ): Declaration =
    Declaration(typeName[A], encoder.value.encode)

  def render[A](
      decls: List[Declaration]
  )(implicit renderer: Renderer[A]): String =
    renderer.render(decls)

  implicit class StringOps(str: String) {
    def :=[A](tpe: Type): Declaration =
      Declaration(str, tpe)
  }

  implicit class StringPairOps(a: (String, Type)) {
    def |(b: (String, Type)): Union =
      discUnion(a, b)
  }
}
