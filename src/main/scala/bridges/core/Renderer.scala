package bridges.core

trait Renderer[A] {
  def render(decls: List[Declaration]): String =
    decls.map(render).mkString("\n\n")

  def render(decl: Declaration): String
}

object Renderer {
  def apply[A](implicit renderer: Renderer[A]): Renderer[A] =
    renderer
}