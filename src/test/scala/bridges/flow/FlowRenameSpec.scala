package bridges.flow

import bridges.SampleTypes._
import bridges.flow.FlowType._
import bridges.flow.syntax._
import org.scalatest._

class FlowRenameSpec extends FreeSpec with Matchers {
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
