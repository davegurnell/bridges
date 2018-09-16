package bridges.core

import bridges.SampleTypes._
import bridges.core.Type._
import bridges.syntax._
import org.scalatest._

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

    "a class with UUID member" in {
      encode[ClassUUID] should be(AProduct("ClassUUID", Struct("a" -> Ref("UUID"))))
    }

    "a class with Date member" in {
      encode[ClassDate] should be(AProduct("ClassDate", Struct("a" -> Ref("Date"))))
    }

    "case classes" in {
      encode[Pair] should be(AProduct("Pair", Struct("a" -> Str, "b" -> Num)))
    }

    "sealed types" in {
      encode[OneOrOther] should be(
        SumOfProducts(
          AProduct("One", Struct("value"   -> Str)),
          AProduct("Other", Struct("value" -> Num))
        )
      )
    }

    "sealed types with objects" in {
      encode[ClassOrObject] should be(
        SumOfProducts(
          AProduct("MyClass", Struct("value" → Num)),
          AProduct("MyObject", Struct(Nil))
        )
      )
    }

    "sealed types with objects in nested objects" in {
      encode[NestedClassOrObject] should be(
        SumOfProducts(
          AProduct("MyClass", Struct("value" → Num)),
          AProduct("MyObject", Struct(Nil))
        )
      )
    }

    "overridden defaults" in {
      implicit val oneEncoder: BasicEncoder[One] =
        Encoder.pure(Str)

      encode[One] should be(Str)
      encode[OneOrOther] should be(
        SumOfProducts(
          AProduct("One", Struct("value"   → Str)),
          AProduct("Other", Struct("value" → Num))
        )
      )
    }

    "sealed types with intermediate types and indirect recursion" in {
      encode[Shape] should be(
        SumOfProducts(
          AProduct(
            "Circle",
            Struct("radius" -> Floating, "color" -> Ref("Color"))
          ),
          AProduct(
            "Rectangle",
            Struct(
              "width"  -> Floating,
              "height" -> Floating,
              "color"  -> Ref("Color")
            )
          ),
          AProduct(
            "ShapeGroup",
            Struct("leftShape" -> Ref("Shape"), "rightShape" -> Ref("Shape"))
          )
        )
      )
      encode[Circle] should be(
        AProduct("Circle", Struct("radius" -> Floating, "color" -> Ref("Color")))
      )
      encode[Rectangle] should be(
        AProduct(
          "Rectangle",
          Struct(
            "width"  -> Floating,
            "height" -> Floating,
            "color"  -> Ref("Color")
          )
        )
      )
      encode[ShapeGroup] should be(
        AProduct("ShapeGroup", Struct("leftShape" -> Ref("Shape"), "rightShape" -> Ref("Shape")))
      )
    }

    "recursive types with direct recursion on same type" in {
      encode[Navigation] should be(
        SumOfProducts(
          AProduct(
            "Node",
            Struct("name" -> Str, "children" -> Array(Ref("Navigation")))
          ),
          AProduct(
            "NodeList",
            Struct("all" -> Array(Ref("Navigation")))
          )
        )
      )
      encode[NodeList] should be(AProduct("NodeList", Struct("all" -> Array(Ref("Navigation")))))
      encode[Node] should be(
        AProduct("Node", Struct("name" -> Str, "children" -> Array(Ref("Navigation"))))
      )
    }

    "types with specific parameters" in {
      encode[Alpha] should be(
        AProduct("Alpha", Struct("name" -> Str, "char" -> Character, "bool" → Bool))
      )
      encode[ArrayClass] should be(
        AProduct("ArrayClass", Struct("aList" -> Array(Str), "optField" -> Optional(Floating)))
      )
      encode[Numeric] should be(
        AProduct("Numeric", Struct("double" -> Floating, "float" -> Floating, "int" → Num))
      )
    }

    "class that references other case classes" in {
      encode[ExternalReferences] should be(
        AProduct("ExternalReferences", Struct("color" -> Ref("Color"), "nav" -> Ref("Navigation")))
      )
    }

    "pure objects ADT" in {
      encode[ObjectsOnly] should be(
        SumOfProducts(
          AProduct(
            "ObjectOne",
            Struct()
          ),
          AProduct(
            "ObjectTwo",
            Struct()
          )
        )
      )
    }

    "refined types and class containing them" in {
      encode[RefinedString] should be(Str)
      encode[RefinedInt] should be(Num)
      encode[RefinedChar] should be(Character)
//      encode[ClassWithRefinedType] should be(AProduct("ClassWithRefinedType", Struct("name" -> Str)))
      //TODO fix this error
    }

    "we can override uuid as string" in {
      implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
        Encoder.pure(Str)

      encode[ClassUUID] should be(
        AProduct("ClassUUID", Struct("a" -> Str))
      )
    }
  }

  "declaration[A]" - {
    "value classes" in {
      declaration[Value] should be("Value" := Str)
    }

    "case classes" in {
      declaration[Pair] should be("Pair" := AProduct("Pair", Struct("a" -> Str, "b" -> Num)))
    }

    "sealed types" in {
      declaration[OneOrOther] should be(
        "OneOrOther" := SumOfProducts(
          AProduct("One", Struct("value"   → Str)),
          AProduct("Other", Struct("value" → Num))
        )
      )
    }

    "overridden defaults" in {
      implicit val oneEncoder: BasicEncoder[One] =
        Encoder.pure(Str)

      encode[One] should be(Str)
      declaration[OneOrOther] should be(
        "OneOrOther" := SumOfProducts(
          AProduct("One", Struct("value"   → Str)),
          AProduct("Other", Struct("value" → Num))
        )
      )
    }

    "class with refined type" in {
      import eu.timepit.refined.shapeless.typeable._
      //TODO fix this error
//      declaration[ClassWithRefinedType] should be(
//        "ClassWithRefinedType" := AProduct("ClassWithRefinedType", Struct("name" -> Str))
//      )
    }
  }
}
