package bridges.core

trait Renderer {
  def render(decl: Decl): String

  def render(decls: List[Decl]): String =
    decls.map(render).mkString("\n\n")
}
