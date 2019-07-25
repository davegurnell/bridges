package bridges.typescript

import bridges.SampleTypes._
import bridges.core.DeclF
import bridges.typescript.TsType._
import bridges.typescript.syntax._
import org.scalatest._
import unindent._

class TsEncoderSpec extends FreeSpec with Matchers {
  "config" - {
    "render optional fields by default" in {
      decl[Recursive] shouldBe {
        decl("Recursive")(
          struct(
            "head" --> Intr,
            "tail" -?> union(ref("Recursive"), Null)
          )
        )
      }
    }

    "override optional fields setting" in {
      implicit val config: TsEncoderConfig =
        TsEncoderConfig(optionalFields = false)

      decl[Recursive] shouldBe {
        decl("Recursive")(
          struct(
            "head" --> Intr,
            "tail" --> union(ref("Recursive"), Null)
          )
        )
      }
    }
  }

  "Numeric types" in {
    decl[NumericTypes] shouldBe {
      decl("NumericTypes")(
        struct(
          "int" --> Intr,
          "long" --> Intr,
          "float" --> Real,
          "double" --> Real,
          "bigDecimal" --> Real
        )
      )
    }
  }
}
