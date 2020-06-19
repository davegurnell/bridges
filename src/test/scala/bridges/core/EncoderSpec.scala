package bridges.core

import bridges.SampleTypes._
import bridges.core.Type._
import bridges.core.syntax._
import org.scalatest._
import shapeless.{ Generic, LabelledGeneric, TypeCase, Typeable }
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EncoderSpec extends AnyFreeSpec with Matchers {
  "encode[A]" - {
    "primitive types" in {
      encode[String] should be(Str)
      encode[Char] should be(Chr)
      encode[Int] should be(Intr)
      encode[Float] should be(Real)
      encode[Double] should be(Real)
      encode[Boolean] should be(Bool)
    }

    "options" in {
      encode[Option[String]] should be(Opt(Str))
      encode[Option[Int]] should be(Opt(Intr))
    }

    "sequences" in {
      encode[Seq[String]] should be(Arr(Str))
      encode[Set[Set[Int]]] should be(Arr(Arr(Intr)))
    }

    "value classes" in {
      encode[Value] should be(Str)
    }

    "a class with UUID member" in {
      encode[ClassUUID] should be(
        prod(
          "a" -> Ref("UUID")
        )
      )
    }

    "a class with Date member" in {
      encode[ClassDate] should be(
        prod(
          "a" -> Ref("Date")
        )
      )
    }

    "case classes" in {
      encode[Pair] should be(
        prod(
          "a" -> Str,
          "b" -> Intr
        )
      )
    }

    "sealed types" in {
      encode[OneOrOther] should be(
        sum(
          "One"   -> prod("value" -> Str),
          "Other" -> prod("value" -> Intr)
        )
      )
    }

    "sealed types with objects" in {
      encode[ClassOrObject] should be(
        sum(
          "MyClass"  -> prod("value" -> Intr),
          "MyObject" -> prod()
        )
      )
    }

    "sealed types with objects in nested objects" in {
      encode[NestedClassOrObject] should be(
        sum(
          "MyClass"  -> prod("value" -> Intr),
          "MyObject" -> prod()
        )
      )
    }

    "overridden defaults" in {
      implicit val oneEncoder: BasicEncoder[One] =
        Encoder.pure(Str)

      encode[One] should be(Str)

      encode[OneOrOther] should be(
        sum(
          "One"   -> prod("value" -> Str),
          "Other" -> prod("value" -> Intr)
        )
      )
    }

    "sealed types with intermediate types and indirect recursion" in {
      encode[Shape] should be(
        sum(
          "Circle" -> prod(
            "radius" -> Real,
            "color"  -> Ref("Color")
          ),
          "Rectangle" -> prod(
            "width"  -> Real,
            "height" -> Real,
            "color"  -> Ref("Color")
          ),
          "ShapeGroup" -> prod(
            "leftShape"  -> Ref("Shape"),
            "rightShape" -> Ref("Shape")
          )
        )
      )

      encode[Circle] should be(
        prod(
          "radius" -> Real,
          "color"  -> Ref("Color")
        )
      )

      encode[Rectangle] should be(
        prod(
          "width"  -> Real,
          "height" -> Real,
          "color"  -> Ref("Color")
        )
      )

      encode[ShapeGroup] should be(
        prod(
          "leftShape"  -> Ref("Shape"),
          "rightShape" -> Ref("Shape")
        )
      )
    }

    "recursive types with direct recursion on same type" in {
      encode[Navigation] should be(
        sum(
          "Node" -> prod(
            "name"     -> Str,
            "children" -> Arr(Ref("Navigation"))
          ),
          "NodeList" -> prod(
            "all" -> Arr(Ref("Navigation"))
          )
        )
      )

      encode[NodeList] should be(
        prod(
          "all" -> Arr(Ref("Navigation"))
        )
      )

      encode[Node] should be(
        prod(
          "name"     -> Str,
          "children" -> Arr(Ref("Navigation"))
        )
      )
    }

    "types with specific parameters" in {
      encode[Alpha] should be(
        prod(
          "name" -> Str,
          "char" -> Chr,
          "bool" -> Bool
        )
      )

      encode[ArrayClass] should be(
        prod(
          "aList"    -> Arr(Str),
          "optField" -> Opt(Real)
        )
      )
      encode[Numeric] should be(
        prod(
          "double" -> Real,
          "float"  -> Real,
          "int"    -> Intr
        )
      )
    }

    "class that references other case classes" in {
      encode[ExternalReferences] should be(
        prod(
          "color" -> Ref("Color"),
          "nav"   -> Ref("Navigation")
        )
      )
    }

    "mutually recursive types" in {
      encode[TypeOne] should be(
        prod(
          "name"   -> Str,
          "values" -> Arr(Ref("TypeTwo"))
        )
      )

      encode[TypeTwo] should be(
        sum(
          "OptionOne" -> prod("value" -> Intr),
          "OptionTwo" -> prod("value" -> Ref("TypeOne"))
        )
      )
    }

    "self-recursive type" in {
      encode[Recursive] should be(
        prod(
          "head" -> Intr,
          "tail" -> Opt(Ref("Recursive"))
        )
      )

      encode[Recursive2] should be(
        prod(
          "head" -> Intr,
          "tail" -> Arr(Ref("Recursive2"))
        )
      )
    }

    "pure objects ADT" in {
      encode[ObjectsOnly] should be(
        sum(
          "ObjectOne" -> prod(),
          "ObjectTwo" -> prod()
        )
      )
    }

    "refined types and class containing them" in {
      encode[RefinedString] should be(Str)
      encode[RefinedInt] should be(Intr)
      encode[RefinedChar] should be(Chr)

      //Note that the import is required or it fails!
      import eu.timepit.refined.shapeless.typeable._
      encode[ClassWithRefinedType] should be(
        prod("name" -> Str)
      )
    }

    "we can override uuid as string" in {
      implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
        Encoder.pure(Str)

      encode[ClassUUID] should be(
        prod("a" -> Str)
      )
    }
  }

  "decl[A]" - {
    "value classes" in {
      decl[Value] should be(
        "Value" := Str
      )
    }

    "case classes" in {
      decl[Pair] should be(
        "Pair" := prod(
            "a" -> Str,
            "b" -> Intr
          )
      )
    }

    "sealed types" in {
      decl[OneOrOther] should be(
        "OneOrOther" := sum(
            "One"   -> prod("value" -> Str),
            "Other" -> prod("value" -> Intr)
          )
      )
    }

    "overridden defaults" in {
      implicit val oneEncoder: BasicEncoder[One] =
        Encoder.pure(Str)

      encode[One] should be(Str)

      decl[OneOrOther] should be(
        "OneOrOther" := sum(
            "One" -> prod(
              "value" -> Str
            ),
            "Other" -> prod(
              "value" -> Intr
            )
          )
      )
    }

    "class with refined type" in {
      // Note that the import is required or it fails!
      import eu.timepit.refined.shapeless.typeable._

      decl[ClassWithRefinedType] should be(
        "ClassWithRefinedType" := prod(
            "name" -> Str
          )
      )
    }
  }

  "Numeric types" in {
    decl[NumericTypes] shouldBe {
      decl("NumericTypes")(
        prod(
          "int"        -> Intr,
          "long"       -> Intr,
          "float"      -> Real,
          "double"     -> Real,
          "bigDecimal" -> Real
        )
      )
    }
  }

  "Map" in {
    decl[Map[String, Int]] shouldBe decl("Map")(dict(Str, Intr))
    decl[Map[String, Pair]] shouldBe decl("Map")(
      dict(
        Str,
        prod(
          "a" -> Str,
          "b" -> Intr
        )
      )
    )
  }
}
