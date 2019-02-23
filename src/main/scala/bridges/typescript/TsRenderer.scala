package bridges.typescript

import bridges.core.Renderer
import unindent._

abstract class TsRenderer(
    typeRenderer: Renderer[TsType],
    guardRenderer: Renderer[TsType]
) extends Renderer[TsType] {
  def render(decl: TsDecl): String =
    i"""
    ${typeRenderer.render(decl)}

    ${guardRenderer.render(decl)}
    """
}
