package bridges.elm

import bridges.core._

sealed trait Elm

object Elm extends ElmJsonDecoder
  with ElmJsonEncoder
  with ElmRenderer
  with ElmFileBuilder
