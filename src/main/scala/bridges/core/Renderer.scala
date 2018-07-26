package bridges.core

trait Renderer[A] {
  def render(decl: Declaration): String

  def render(decls: List[Declaration]): String =
    decls.map(render).mkString("\n\n")
}

object Renderer {
  def apply[A](implicit renderer: Renderer[A]): Renderer[A] =
    renderer
}
