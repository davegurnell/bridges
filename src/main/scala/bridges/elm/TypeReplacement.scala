package bridges.elm

final case class TypeReplacement(newType: String, imports: Option[String], decoder: Option[String], encoder: Option[String])

object TypeReplacement {
  def apply(newType: String): TypeReplacement = TypeReplacement(newType, None, None, None)

  def apply(newType: String, imports: String, decoder: String, encoder: String): TypeReplacement =
    new TypeReplacement(newType, Some(imports), Some(decoder), Some(encoder))
}
