package bridges

import bridges.core._
import shapeless.{Lazy, Typeable}

object syntax {
  import Type._

  def encode[A: Encoder]: Type =
    Encoder[A].encode

  // latest Shapeless version adds typeable information of members to case classes, as well as to objects, which we don't want. Filtering that out while I discover a better fix
  def typeName[A](implicit typeable: Typeable[A]): String =
    typeable.describe.takeWhile(c ⇒ c != '[' && c != '.').mkString

  def declaration[A](
      implicit typeable: Typeable[A],
      encoder: Lazy[Encoder[A]]
  ): Declaration =
    Declaration(typeName[A], encoder.value.encode)

  def render[A](decl: Declaration)(implicit renderer: Renderer[A]): String =
    renderer.render(decl)

  def render[A](
      decls: List[Declaration]
  )(implicit renderer: Renderer[A]): String =
    renderer.render(decls)

  def jsonDecoder[A](
      decls: List[Declaration]
  )(implicit jDecoder: JsonDecoder[A]): String =
    jDecoder.decoder(decls)

  def jsonDecoder[A](
      decl: Declaration
  )(implicit jDecoder: JsonDecoder[A]): String =
    jDecoder.decoder(decl)

  def jsonEncoder[A](
      decls: List[Declaration]
  )(implicit jEncoder: JsonEncoder[A]): String =
    jEncoder.encoder(decls)

  def jsonEncoder[A](
      decl: Declaration
  )(implicit jEncoder: JsonEncoder[A]): String =
    jEncoder.encoder(decl)

  def buildFile[A](module: String, decls: List[Declaration])(
      implicit fBuilder: FileBuilder[A]
  ): (String, String) =
    fBuilder.buildFile(module, decls)

  def buildFile[A](module: String, decl: Declaration)(
      implicit fBuilder: FileBuilder[A]
  ): (String, String) =
    fBuilder.buildFile(module, decl)

  implicit class StringOps(str: String) {
    def :=[A](tpe: Type): Declaration =
      Declaration(str, tpe)
  }

  implicit class StringPairOps(a: (String, Type, Struct)) {
    def |(b: (String, Type, Struct)): Union =
      discUnion(a, b)
  }
}
