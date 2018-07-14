package bridges.core

trait FileBuilder[A] {
  // Given a declaration, returns a tuple with file name and file contents:
  def buildFile(module: String, decl: Declaration): (String, String) =
    buildFile(module, List(decl))

  def buildFile(module: String, decls: List[Declaration]): (String, String)
}
