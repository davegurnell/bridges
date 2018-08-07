package bridges.core

trait Renderer {
  def render(decl: Declaration): String

  def render(decls: List[Declaration]): String =
    decls.map(render).mkString("\n\n")
}
