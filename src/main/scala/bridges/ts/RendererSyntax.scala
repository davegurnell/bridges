package bridges.ts

trait RendererSyntax {
  import Type._

  implicit class BindingListRenderOps(bindings: List[Binding]) {
    def render: String =
      Renderer.render(bindings)
  }
}
