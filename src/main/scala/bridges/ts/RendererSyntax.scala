package bridges.ts

trait RendererSyntax {
  import TsType._

  implicit class BindingListRenderOps(bindings: List[Binding]) {
    def render: String =
      Renderer.render(bindings)
  }
}
