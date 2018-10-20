package bridges.elm

import bridges.SampleTypes._
import bridges.core.Type._
import bridges.core._
import bridges.core.syntax._
import org.scalatest._

class ElmRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    Elm.render(decl[Color]) shouldBe "type alias Color = { red: Int, green: Int, blue: Int }"
  }

  "Circle" in {
    Elm.render(decl[Circle]) shouldBe "type alias Circle = { radius: Float, color: Color }"
  }

  "Rectangle" in {
    Elm.render(decl[Rectangle]) shouldBe "type alias Rectangle = { width: Float, height: Float, color: Color }"
  }

  "Shape" in {
    Elm.render(decl[Shape]) shouldBe """type Shape = Circle Float Color | Rectangle Float Float Color | ShapeGroup Shape Shape"""
  }

  "Alpha" in {
    Elm.render(decl[Alpha]) shouldBe "type alias Alpha = { name: String, char: Char, bool: Bool }"
  }

  "ArrayClass" in {
    Elm.render(decl[ArrayClass]) shouldBe """type alias ArrayClass = { aList: (List String), optField: (Maybe Float) }"""
  }

  "Numeric" in {
    Elm.render(decl[Numeric]) shouldBe """type alias Numeric = { double: Float, float: Float, int: Int }"""
  }

  "ClassOrObject" in {
    Elm.render(decl[ClassOrObject]) shouldBe """type ClassOrObject = MyClass Int | MyObject"""
  }

  "NestedClassOrObject" in {
    Elm.render(decl[NestedClassOrObject]) shouldBe """type NestedClassOrObject = MyClass Int | MyObject"""
  }

  "Navigation" in {
    Elm.render(decl[Navigation]) shouldBe """type Navigation = Node String (List Navigation) | NodeList (List Navigation)"""
  }

  "TypeOne and TypeTwo" in {
    Elm.render(decl[TypeOne]) shouldBe """type alias TypeOne = { name: String, values: (List TypeTwo) }"""
    Elm.render(decl[TypeTwo]) shouldBe """type TypeTwo = OptionOne Int | OptionTwo TypeOne"""
  }

  "ExternalReferences" in {
    Elm.render(decl[ExternalReferences]) shouldBe """type alias ExternalReferences = { color: Color, nav: Navigation }"""
  }

  "ClassUUID" - {
    "Without any specific override" in {
      Elm.render(decl[ClassUUID]) shouldBe """type alias ClassUUID = { a: UUID }"""
    }

    "providing a type override map" in {
      val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
        Ref("UUID") → TypeReplacement("Uuid", "import Uuid exposing (Uuid)", "Uuid.decoder", "Uuid.encode")
      )

      Elm.render(decl[ClassUUID], customTypeReplacements) shouldBe """type alias ClassUUID = { a: Uuid }"""
    }

    "using override to treat UUID as a String" in {
      implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
        Encoder.pure(Str)

      Elm.render(decl[ClassUUID]) shouldBe """type alias ClassUUID = { a: String }"""
    }
  }

  "ClassDate" - {
    "Without any specific override" in {
      Elm.render(decl[ClassDate]) shouldBe """type alias ClassDate = { a: Date }"""
    }

    "providing a type override map" in {
      val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
        Ref("Date") → TypeReplacement("Date", "import Date exposing (Date)", "Date.decoder", "Date.encode")
      )

      Elm.render(decl[ClassDate], customTypeReplacements) shouldBe """type alias ClassDate = { a: Date }"""
    }

    "using override to treat Date as a String" in {
      implicit val dateEncoder: BasicEncoder[java.util.Date] =
        Encoder.pure(Str)

      Elm.render(decl[ClassDate]) shouldBe """type alias ClassDate = { a: String }"""
    }
  }

  "ObjectsOnly" in {
    Elm.render(decl[ObjectsOnly]) shouldBe """type ObjectsOnly = ObjectOne | ObjectTwo"""
  }

  "ClassWithRefinedType" in {
    import eu.timepit.refined.shapeless.typeable._

    Elm.render(decl[ClassWithRefinedType]) shouldBe """type alias ClassWithRefinedType = { name: String }"""
  }
}
