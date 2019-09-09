package bridges.typescript

import bridges.SampleTypes._
import bridges.typescript.TsType._
import bridges.typescript.syntax._
import org.scalatest._

class TsRenameSpec extends FreeSpec with Matchers {
  "decl" in {
    val actual = decl[Color].rename("red", "r")

    val expected = "Color" := struct(
      "r" --> Intr,
      "green" --> Intr,
      "blue" --> Intr
    )

    actual shouldBe expected
  }
}
