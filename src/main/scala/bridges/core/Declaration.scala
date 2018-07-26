package bridges.core

final case class Declaration(id: String, tpe: Type) {
  def renameRef(from: String, to: String): Declaration =
    Declaration(if(id == from) to else id, tpe.renameRef(from, to))
}
