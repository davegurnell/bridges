package bridges.core

//TODO: not convinced we can't replace this by AProduct....
final case class Declaration(id: String, tpe: Type) {
  def renameRef(from: String, to: String): Declaration =
    Declaration(if (id == from) to else id, tpe.renameRef(from, to))
}
