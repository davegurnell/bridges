package bridges

import bridges.TsType._
import bridges.syntax._
import bridges.SampleTypes._
import munit.FunSuite

class TsRenameSuite extends FunSuite {
  test("decl") {
    val actual = decl[Color].rename("red", "r")

    val expected = decl("Color")(struct(
      "r"     ---> Num,
      "green" ---> Num,
      "blue"  ---> Num
    ))

    assertEquals(actual, expected)
  }
}
