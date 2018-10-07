package bridges.elm

import bridges.SampleTypes._
import bridges.core.Type._
import bridges.core._
import bridges.syntax._
import org.scalatest._
import shapeless.Typeable

class ElmRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    Elm.render(declaration[Color]) shouldBe "type alias Color = { red: Int, green: Int, blue: Int }"
  }

  "Circle" in {
    Elm.render(declaration[Circle]) shouldBe "type alias Circle = { radius: Float, color: Color }"
  }

  "Rectangle" in {
    Elm.render(declaration[Rectangle]) shouldBe "type alias Rectangle = { width: Float, height: Float, color: Color }"
  }

  "Shape" in {
    Elm.render(declaration[Shape]) shouldBe """type Shape = Circle Float Color | Rectangle Float Float Color | ShapeGroup Shape Shape"""
  }

  "Alpha" in {
    Elm.render(declaration[Alpha]) shouldBe "type alias Alpha = { name: String, char: Char, bool: Bool }"
  }

  "ArrayClass" in {
    Elm.render(declaration[ArrayClass]) shouldBe """type alias ArrayClass = { aList: (List String), optField: (Maybe Float) }"""
  }

  "Numeric" in {
    Elm.render(declaration[Numeric]) shouldBe """type alias Numeric = { double: Float, float: Float, int: Int }"""
  }

  "ClassOrObject" in {
    Elm.render(declaration[ClassOrObject]) shouldBe """type ClassOrObject = MyClass Int | MyObject"""
  }

  "NestedClassOrObject" in {
    Elm.render(declaration[NestedClassOrObject]) shouldBe """type NestedClassOrObject = MyClass Int | MyObject"""
  }

  "Navigation" in {
    Elm.render(declaration[Navigation]) shouldBe """type Navigation = Node String (List Navigation) | NodeList (List Navigation)"""
  }

  "ExternalReferences" in {
    Elm.render(declaration[ExternalReferences]) shouldBe """type alias ExternalReferences = { color: Color, nav: Navigation }"""
  }

  "ClassUUID" - {
    "Without any specific override" in {
      Elm.render(declaration[ClassUUID]) shouldBe """type alias ClassUUID = { a: UUID }"""
    }

    "providing a type override map" in {
      val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
        Ref("UUID") → TypeReplacement("Uuid", "import Uuid exposing (Uuid)", "Uuid.decoder", "Uuid.encode")
      )

      Elm.render(declaration[ClassUUID], customTypeReplacements) shouldBe """type alias ClassUUID = { a: Uuid }"""
    }

    "using override to treat UUID as a String" in {
      implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
        Encoder.pure(Str)

      Elm.render(declaration[ClassUUID]) shouldBe """type alias ClassUUID = { a: String }"""
    }
  }

  "ClassDate" - {
    "Without any specific override" in {
      Elm.render(declaration[ClassDate]) shouldBe """type alias ClassDate = { a: Date }"""
    }

    "providing a type override map" in {
      val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
        Ref("Date") → TypeReplacement("Date", "import Date exposing (Date)", "Date.decoder", "Date.encode")
      )

      Elm.render(declaration[ClassDate], customTypeReplacements) shouldBe """type alias ClassDate = { a: Date }"""
    }

    "using override to treat Date as a String" in {
      implicit val dateEncoder: BasicEncoder[java.util.Date] =
        Encoder.pure(Str)

      Elm.render(declaration[ClassDate]) shouldBe """type alias ClassDate = { a: String }"""
    }
  }

  "ObjectsOnly" in {
    Elm.render(declaration[ObjectsOnly]) shouldBe """type ObjectsOnly = ObjectOne | ObjectTwo"""
  }

  "ClassWithRefinedType" in {
    import eu.timepit.refined.shapeless.typeable._

    Elm.render(declaration[ClassWithRefinedType]) shouldBe """type alias ClassWithRefinedType = { name: String }"""
  }

  "class with parameters" in {
    Elm.render(declaration[ClassWithParams[String, Int]]) shouldBe """type alias ClassWithParams = { param: String, param2: Int }"""
    Elm.render(declaration[ClassWithParams[Alpha, ArrayClass]]) shouldBe """type alias ClassWithParams = { param: Alpha, param2: ArrayClass }"""

    class Sample[A, B] {
      // without the implicits below, the typer can't find the instances and we get a diverging implicit expansion error
      implicit val aTypeable: Typeable[A] = new Typeable[A] {
        override def cast(t: Any): Option[A] = None
        override def describe: String        = "A"
      }
      implicit val bTypeable: Typeable[B] = new Typeable[B] {
        override def cast(t: Any): Option[B] = None
        override def describe: String        = "B"
      }

      // We can use the implicit typeable definitions above to tweak the result or just assign type to the params before encoding.
      Elm.render(declaration[ClassWithParams[A, B]]) shouldBe """type alias ClassWithParams = { param: A, param2: B }"""
    }
  }
}
