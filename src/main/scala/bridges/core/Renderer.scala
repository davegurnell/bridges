package bridges.core

trait Renderer[A] {
  def render(decl: DeclF[A]): String

  def render(decls: List[DeclF[A]]): String =
    decls.map(render).mkString("\n\n")
}
