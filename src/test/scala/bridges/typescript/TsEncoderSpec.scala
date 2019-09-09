package bridges.typescript

import bridges.SampleTypes._
import bridges.core.DeclF
import bridges.typescript.TsType._
import bridges.typescript.syntax._
import org.scalatest._
import unindent._

class TsEncoderSpec extends FreeSpec with Matchers {
  "TsEncoderConfig.optionalFields" - {
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

    "override setting" in {
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

  "TsEncoderConfig.refsUnions" - {
    "render type names in unions by default" in {
      decl[OneOrOther] shouldBe {
        decl("OneOrOther")(ref("One") | ref("Other"))
      }
    }

    "override setting" in {
      implicit val config: TsEncoderConfig =
        TsEncoderConfig(refsInUnions = false)

      decl[OneOrOther] shouldBe {
        decl("OneOrOther")(
          struct(
            "type" --> StrLit("One"),
            "value" --> Str
          ) |
          struct(
            "type" --> StrLit("Other"),
            "value" --> Intr
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
