// package bridges.typescript
//
// import bridges.SampleTypes._
// import bridges.typescript.TsType._
// import org.scalatest._
// import unindent._
// import org.scalatest.freespec.AnyFreeSpec
// import org.scalatest.matchers.should.Matchers
//
// class TsEncoderSpec extends AnyFreeSpec with Matchers {
//   "TsEncoderConfig.optionalFields" - {
//     "render optional fields by default" in {
//       decl[Recursive] shouldBe {
//         decl("Recursive")(
//           struct(
//             "head" --> Intr,
//             "tail" -?> union(ref("Recursive"), Null)
//           )
//         )
//       }
//     }
//
//     "override setting" in {
//       implicit val config: TsEncoderConfig =
//         TsEncoderConfig(optionalFields = false)
//
//       decl[Recursive] shouldBe {
//         decl("Recursive")(
//           struct(
//             "head" --> Intr,
//             "tail" --> union(ref("Recursive"), Null)
//           )
//         )
//       }
//     }
//   }
//
//   "TsEncoderConfig.refsUnions" - {
//     "render type names in unions by default" in {
//       decl[OneOrOther] shouldBe {
//         decl("OneOrOther")(
//           struct(
//             "type" --> StrLit("One"),
//             "value" --> Str
//           ) |
//             struct(
//               "type" --> StrLit("Other"),
//               "value" --> Intr
//             )
//         )
//       }
//     }
//
//     "override setting" in {
//       implicit val config: TsEncoderConfig =
//         TsEncoderConfig(refsInUnions = true)
//
//       decl[OneOrOther] shouldBe {
//         decl("OneOrOther")(
//           (struct("type" --> StrLit("One")) & ref("One")) |
//             (struct("type" --> StrLit("Other")) & ref("Other"))
//         )
//       }
//     }
//   }
//
//   "Numeric types" in {
//     decl[NumericTypes] shouldBe {
//       decl("NumericTypes")(
//         struct(
//           "int" --> Intr,
//           "long" --> Intr,
//           "float" --> Real,
//           "double" --> Real,
//           "bigDecimal" --> Real
//         )
//       )
//     }
//   }
// }
