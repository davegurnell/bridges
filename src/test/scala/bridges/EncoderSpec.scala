package bridges

import org.scalatest._
import SampleTypes._
import syntax._
import Type._

class EncoderSpec extends FreeSpec with Matchers {

  "encode[A]" - {
    "primitive types" in {
      encode[String] should be(Str)
      encode[Char] should be(Character)
      encode[Int] should be(Num)
      encode[Float] should be(Floating)
      encode[Double] should be(Floating)
      encode[Boolean] should be(Bool)
    }

    "options" in {
      encode[Option[String]] should be(Optional(Str))
      encode[Option[Int]] should be(Optional(Num))
    }

    "sequences" in {
      encode[Seq[String]] should be(Array(Str))
      encode[Set[Set[Int]]] should be(Array(Array(Num)))
    }

    "value classes" in {
      encode[Value] should be(Str)
    }

    "case classes" in {
      encode[Pair] should be(Struct("a" -> Str, "b" -> Num))
    }

    "sealed types" in {
      encode[OneOrOther] should be(
        discUnion(
          ("One", Ref("One"), Struct("value" -> Str)),
          ("Other", Ref("Other"), Struct("value" -> Num))
        )
      )
    }

    "sealed types with objects" in {
      encode[ClassOrObject] should be(
        discUnion(
          ("MyClass", Ref("MyClass"), Struct("value" → Num)),
          ("MyObject", Ref("MyObject"), Struct(Nil))
        )
      )
    }

    "overridden defaults" in {
      implicit val oneEncoder: BasicEncoder[One] =
        Encoder.pure(Str)

      encode[One] should be(Str)
      encode[OneOrOther] should be(
        discUnion(
          ("One", Str, Struct("value" → Str)),
          ("Other", Ref("Other"), Struct("value" → Num))
        )
      )
    }

    "sealed types with intermediate types and indirect recursion" in {
      encode[Shape] should be(
        discUnion(
          (
            "Circle",
            Ref("Circle"),
            Struct("radius" -> Floating, "color" -> Ref("Color"))
          ),
          (
            "Rectangle",
            Ref("Rectangle"),
            Struct(
              "width" -> Floating,
              "height" -> Floating,
              "color" -> Ref("Color")
            )
          ),
          (
            "ShapeGroup",
            Ref("ShapeGroup"),
            Struct("leftShape" -> Ref("Shape"), "rightShape" -> Ref("Shape"))
          )
        )
      )
      encode[Circle] should be(
        Struct("radius" -> Floating, "color" -> Ref("Color"))
      )
      encode[Rectangle] should be(
        Struct(
          "width" -> Floating,
          "height" -> Floating,
          "color" -> Ref("Color")
        )
      )
      encode[ShapeGroup] should be(
        Struct("leftShape" -> Ref("Shape"), "rightShape" -> Ref("Shape"))
      )
    }

    "recursive types with direct recursion on same type" in {
      encode[Navigation] should be(
        discUnion(
          (
            "Node",
            Ref("Node"),
            Struct("name" -> Str, "children" -> Array(Ref("Navigation")))
          ),
          (
            "NodeList",
            Ref("NodeList"),
            Struct("all" -> Array(Ref("Navigation")))
          )
        )
      )
      encode[NodeList] should be(Struct("all" -> Array(Ref("Navigation"))))
      encode[Node] should be(
        Struct("name" -> Str, "children" -> Array(Ref("Navigation")))
      )
    }

    "types with specific parameters" in {
      encode[Alpha] should be(
        Struct("name" -> Str, "char" -> Character, "bool" → Bool)
      )
      encode[ArrayClass] should be(
        Struct("aList" -> Array(Str), "optField" -> Optional(Floating))
      )
      encode[Numeric] should be(
        Struct("double" -> Floating, "float" -> Floating, "int" → Num)
      )
    }
  }

  "declaration[A]" - {
    "value classes" in {
      declaration[Value] should be("Value" := Str)
    }

    "case classes" in {
      declaration[Pair] should be("Pair" := Struct("a" -> Str, "b" -> Num))
    }

    "sealed types" in {
      declaration[OneOrOther] should be(
        "OneOrOther" := discUnion(
          ("One", Ref("One"), Struct("value" → Str)),
          ("Other", Ref("Other"), Struct("value" → Num))
        )
      )
    }

    "overridden defaults" in {
      implicit val oneEncoder: BasicEncoder[One] =
        Encoder.pure(Str)

      encode[One] should be(Str)
      declaration[OneOrOther] should be(
        "OneOrOther" := discUnion(
          ("One", Str, Struct("value" → Str)),
          ("Other", Ref("Other"), Struct("value" → Num))
        )
      )
    }
  }
}
