package bridges.core

import bridges.core.Type._
import org.scalatest._

class TypeSpec extends FreeSpec with Matchers {
  "type.renameRef" - {
    "matching Ref" in {
      val actual   = Ref("foo").renameRef("foo", "bar")
      val expected = Ref("bar")
      actual should equal(expected)
    }

    "non-matching Ref" in {
      val actual   = Ref("baz").renameRef("foo", "bar")
      val expected = Ref("baz")
      actual should equal(expected)
    }

    "Optional" in {
      val actual   = Optional(Ref("foo")).renameRef("foo", "bar")
      val expected = Optional(Ref("bar"))
      actual should equal(expected)
    }

    "Array" in {
      val actual   = Array(Ref("foo")).renameRef("foo", "bar")
      val expected = Array(Ref("bar"))
      actual should equal(expected)
    }

    "Struct" in {
      val actual = Struct(
        List(
          "a" -> Ref("foo"),
          "b" -> Ref("baz")
        )
      ).renameRef("foo", "bar")

      val expected = Struct(
        List(
          "a" -> Ref("bar"),
          "b" -> Ref("baz")
        )
      )

      actual should equal(expected)
    }

    "AProduct" - {
      "renames members " in {
        val actual = AProduct(
          "typeA",
          Struct(
            List(
              "a" -> Ref("foo"),
              "b" -> Ref("baz")
            )
          )
        ).renameRef("foo", "bar")

        val expected = AProduct(
          "typeA",
          Struct(
            List(
              "a" -> Ref("bar"),
              "b" -> Ref("baz")
            )
          )
        )

        actual should equal(expected)
      }
      "renames type name" in {
        val actual = AProduct(
          "typeA",
          Struct(
            List(
              "a" -> Ref("foo"),
              "b" -> Ref("baz")
            )
          )
        ).renameRef("typeA", "typeB")

        val expected = AProduct(
          "typeB",
          Struct(
            List(
              "a" -> Ref("foo"),
              "b" -> Ref("baz")
            )
          )
        )

        actual should equal(expected)
      }
    }

    "SumOfProducts" - {
      "renames members " in {
        val actual =
          SumOfProducts(
            AProduct(
              "typeA",
              Struct(
                List(
                  "a" -> Ref("foo"),
                  "b" -> Ref("baz")
                )
              )
            ),
            AProduct(
              "typeB",
              Struct(
                List(
                  "a" -> Ref("foo"),
                  "b" -> Ref("baz")
                )
              )
            )
          ).renameRef("foo", "bar")

        val expected = SumOfProducts(
          AProduct(
            "typeA",
            Struct(
              List(
                "a" -> Ref("bar"),
                "b" -> Ref("baz")
              )
            )
          ),
          AProduct(
            "typeB",
            Struct(
              List(
                "a" -> Ref("bar"),
                "b" -> Ref("baz")
              )
            )
          )
        )

        actual should equal(expected)
      }
      "renames type name of members" in {
        val actual =
          SumOfProducts(
            AProduct(
              "typeA",
              Struct(
                List(
                  "a" -> Ref("foo"),
                  "b" -> Ref("baz")
                )
              )
            ),
            AProduct(
              "typeA",
              Struct(
                List(
                  "a" -> Ref("foo"),
                  "b" -> Ref("baz")
                )
              )
            )
          ).renameRef("typeA", "typeC")

        val expected = SumOfProducts(
          AProduct(
            "typeC",
            Struct(
              List(
                "a" -> Ref("foo"),
                "b" -> Ref("baz")
              )
            )
          ),
          AProduct(
            "typeC",
            Struct(
              List(
                "a" -> Ref("foo"),
                "b" -> Ref("baz")
              )
            )
          )
        )

        actual should equal(expected)
      }
    }
  }
}
