package bridges.core

object syntax extends RenamableSyntax {
  import Type._

  def encode[A: Encoder]: Type =
    Encoder[A].encode

  inline def decl[A](implicit encoder: => Encoder[A]): Decl =
    DeclF(TypeName.getTypeName[A], encoder.encode)

  def decl(name: String, params: String*)(tpe: Type): Decl =
    DeclF(name, params.toList, tpe)

  def prod(fields: (String, Type)*): Prod =
    Prod(fields.toList)

  def sum(products: (String, Prod)*): Sum =
    Sum(products.toList)

  def dict(keyType: Type, valueType: Type): Type =
    Dict(keyType, valueType)

  implicit class StringDeclOps(str: String) {
    def :=[A](tpe: A): DeclF[A] =
      DeclF(str, tpe)
  }
}
