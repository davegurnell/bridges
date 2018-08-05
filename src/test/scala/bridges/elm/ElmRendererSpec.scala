package bridges.elm

import bridges.SampleTypes._
import bridges.core.Type._
import bridges.core._
import bridges.syntax._
import org.scalatest._

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

  "ClassUUID" in {
    Elm.render(declaration[ClassUUID]) shouldBe """type alias ClassUUID = { a: Uuid }"""
  }

  "ExternalReferences" in {
    Elm.render(declaration[ExternalReferences]) shouldBe """type alias ExternalReferences = { color: Color, nav: Navigation }"""
  }

  "MyUUID" in {
    // we want to treat UUID as string, using an override
    implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
      Encoder.pure(Str)

    Elm.render(declaration[MyUUID]) shouldBe """type alias MyUUID = { uuid: String }"""
  }

  "ObjectsOnly" in {
    Elm.render(declaration[ObjectsOnly]) shouldBe """type ObjectsOnly = ObjectOne | ObjectTwo"""
  }

  "ClassWithRefinedType" in {
    import eu.timepit.refined.shapeless.typeable._

    Elm.render(declaration[ClassWithRefinedType]) shouldBe """type alias ClassWithRefinedType = { name: String }"""
  }
}
