package bridges.flow

import bridges.SampleTypes._
import bridges.flow.FlowType._
import bridges.flow.syntax._
import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FlowRenameSpec extends AnyFreeSpec with Matchers {
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
