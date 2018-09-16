package bridges.core

//TODO: is declaration needed if a product has its own name?
final case class Declaration(id: String, tpe: Type) {
  def renameRef(from: String, to: String): Declaration =
    Declaration(if (id == from) to else id, tpe.renameRef(from, to))
}
