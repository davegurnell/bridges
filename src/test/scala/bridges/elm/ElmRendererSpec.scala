package bridges.elm

import bridges.SampleTypes._
import bridges.core._
import bridges.core.Type._
import bridges.syntax._
import shapeless.Typeable
import org.scalatest._

class ElmRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    render[Elm](declaration[Color]) shouldBe "type alias Color = { red: Int, green: Int, blue: Int }"
  }

  "Circle" in {
    render[Elm](declaration[Circle]) shouldBe "type alias Circle = { radius: Float, color: Color }"
  }

  "Rectangle" in {
    render[Elm](declaration[Rectangle]) shouldBe "type alias Rectangle = { width: Float, height: Float, color: Color }"
  }

  "Shape" in {
    render[Elm](declaration[Shape]) shouldBe """type Shape = Circle Float Color | Rectangle Float Float Color | ShapeGroup Shape Shape"""
  }

  "Alpha" in {
    render[Elm](declaration[Alpha]) shouldBe "type alias Alpha = { name: String, char: Char, bool: Bool }"
  }

  "ArrayClass" in {
    render[Elm](declaration[ArrayClass]) shouldBe """type alias ArrayClass = { aList: (List String), optField: (Maybe Float) }"""
  }

  "Numeric" in {
    render[Elm](declaration[Numeric]) shouldBe """type alias Numeric = { double: Float, float: Float, int: Int }"""
  }

  "ClassOrObject" in {
    render[Elm](declaration[ClassOrObject]) shouldBe """type ClassOrObject = MyClass Int | MyObject"""
  }

  "NestedClassOrObject" in {
    render[Elm](declaration[NestedClassOrObject]) shouldBe """type NestedClassOrObject = MyClass Int | MyObject"""
  }

  "Navigation" in {
    render[Elm](declaration[Navigation]) shouldBe """type Navigation = Node String (List Navigation) | NodeList (List Navigation)"""
  }

  "ClassUUID" in {
    render[Elm](declaration[ClassUUID]) shouldBe """type alias ClassUUID = { a: Uuid }"""
  }

  "ExternalReferences" in {
    render[Elm](declaration[ExternalReferences]) shouldBe """type alias ExternalReferences = { color: Color, nav: Navigation }"""
  }

  "MyUUID" in {
    // we want to treat UUID as string, using an override
    implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
      Encoder.pure(Str)

    render[Elm](declaration[MyUUID]) shouldBe """type alias MyUUID = { uuid: String }"""
  }

  "ObjectsOnly" in {
    render[Elm](declaration[ObjectsOnly]) shouldBe """type ObjectsOnly = ObjectOne | ObjectTwo"""
  }

  "ClassWithRefinedType" in {
    import eu.timepit.refined._

    implicit val refinedTypeEncoder: BasicEncoder[ShortString] =
      Encoder.pure(Str)

    implicit val refinedTypeTypeable: Typeable[ShortString] =
      new Typeable[ShortString] {
        def cast(t: Any): Option[ShortString] = {
          if (t != null && t.isInstanceOf[String])
            refineV[ShortStringRefinementType](t.asInstanceOf[String]).toOption
          else None
        }
        def describe: String = "ShortString"
      }

    render[Elm](declaration[ClassWithRefinedType]) shouldBe """type alias ClassWithRefinedType = { name: String }"""
  }
}
