package bridges.core

import bridges.core.syntax._
import org.scalatest._

class TypeSpec extends FreeSpec with Matchers {
  import Type._

  def t[A <: Type](a: A): Type = a

  "type.rename" - {
    "matching Ref" in {
      val actual   = t(Ref("foo")).rename("foo", "bar")
      val expected = t(Ref("bar"))
      actual should equal(expected)
    }

    "non-matching Ref" in {
      val actual   = t(Ref("baz")).rename("foo", "bar")
      val expected = t(Ref("baz"))
      actual should equal(expected)
    }

    "Optional" in {
      val actual   = t(Opt(Ref("foo"))).rename("foo", "bar")
      val expected = t(Opt(Ref("bar")))
      actual should equal(expected)
    }

    "Array" in {
      val actual   = t(Arr(Ref("foo"))).rename("foo", "bar")
      val expected = t(Arr(Ref("bar")))
      actual should equal(expected)
    }

    "Prod" in {
      val actual = t(
        prod(
          "a" := Ref("foo"),
          "b" := Ref("baz")
        )
      ).rename("foo", "bar")

      val expected = t(
        prod(
          "a" := Ref("bar"),
          "b" := Ref("baz")
        )
      )

      actual should equal(expected)
    }

    "Sum" - {
      "renames members " in {
        val actual = t(
          sum(
            "typeA" := prod(
              "a" := Ref("foo"),
              "b" := Ref("baz")
            ),
            "typeB" := prod(
              "a" := Ref("foo"),
              "b" := Ref("baz")
            )
          )
        ).rename("foo", "bar")

        val expected = t(
          sum(
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
        )

        actual should equal(expected)
      }
      "renames type name of members" in {
        val actual = t(
          sum(
            "typeA" := prod(
              "a" := Ref("foo"),
              "b" := Ref("baz")
            ),
            "typeA" := prod(
              "a" := Ref("foo"),
              "b" := Ref("baz")
            )
          )
        ).rename("typeA", "typeC")

        val expected = t(
          sum(
            "typeC" := prod(
              "a" := Ref("foo"),
              "b" := Ref("baz")
            ),
            "typeC" := prod(
              "a" := Ref("foo"),
              "b" := Ref("baz")
            )
          )
        )

        actual should equal(expected)
      }
    }
  }
}
