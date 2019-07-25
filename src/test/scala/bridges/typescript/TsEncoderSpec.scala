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

    "ApiVersion regression test" in {
      case class ServerStatus(
          version: String,
          uptime: Int
      )

      decl[ServerStatus] shouldBe {
        decl("ServerStatus")(
          struct(
            "version" --> Str,
            "uptime" --> Intr
          )
        )
      }
    }
  }

}
