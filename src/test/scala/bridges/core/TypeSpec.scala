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

    "StrLiteral" in {
      val actual   = StrLiteral("foo").renameRef("foo", "bar")
      val expected = StrLiteral("foo")
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

    "Intersection" in {
      val actual = Intersection(
        Struct(
          List(
            "a" -> Ref("foo"),
            "b" -> Ref("baz")
          )
        ),
        Ref("foo"),
        Struct(
          List(
            "a" -> Ref("foo"),
            "b" -> Ref("baz")
          )
        )
      ).renameRef("foo", "bar")

      val expected = Intersection(
        Struct(
          List(
            "a" -> Ref("bar"),
            "b" -> Ref("baz")
          )
        ),
        Ref("bar"),
        Struct(
          List(
            "a" -> Ref("bar"),
            "b" -> Ref("baz")
          )
        )
      )

      actual should equal(expected)
    }
  }
}
