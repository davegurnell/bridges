package bridges.core

import bridges.core.syntax._
import org.scalatest._

class TypeSpec extends FreeSpec with Matchers {
  import Type._

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
      val actual   = Opt(Ref("foo")).renameRef("foo", "bar")
      val expected = Opt(Ref("bar"))
      actual should equal(expected)
    }

    "Array" in {
      val actual   = Arr(Ref("foo")).renameRef("foo", "bar")
      val expected = Arr(Ref("bar"))
      actual should equal(expected)
    }

    "Prod" in {
      val actual = prod(
        "a" := Ref("foo"),
        "b" := Ref("baz")
      ).renameRef("foo", "bar")

      val expected = prod(
        "a" := Ref("bar"),
        "b" := Ref("baz")
      )

      actual should equal(expected)
    }

    "Sum" - {
      "renames members " in {
        val actual = sum(
          "typeA" := prod(
            "a" := Ref("foo"),
            "b" := Ref("baz")
          ),
          "typeB" := prod(
            "a" := Ref("foo"),
            "b" := Ref("baz")
          )
        ).renameRef("foo", "bar")

        val expected = sum(
          "typeA" := Prod(
            List(
              "a" := Ref("bar"),
              "b" := Ref("baz")
            )
          ),
          "typeB" := Prod(
            List(
              "a" := Ref("bar"),
              "b" := Ref("baz")
            )
          )
        )

        actual should equal(expected)
      }
      "renames type name of members" in {
        val actual = sum(
          "typeA" := prod(
            "a" := Ref("foo"),
            "b" := Ref("baz")
          ),
          "typeA" := prod(
            "a" := Ref("foo"),
            "b" := Ref("baz")
          )
        ).renameRef("typeA", "typeC")

        val expected = sum(
          "typeC" := prod(
            "a" := Ref("foo"),
            "b" := Ref("baz")
          ),
          "typeC" := prod(
            "a" := Ref("foo"),
            "b" := Ref("baz")
          )
        )

        actual should equal(expected)
      }
    }
  }
}
