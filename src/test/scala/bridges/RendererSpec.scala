package bridges

import org.scalatest._
import syntax._
import types.SampleTypes._
import bridges.Type.Str
import shapeless.Typeable

class RendererSpec extends FreeSpec with Matchers {

  "render" - {
    "typescript" - {

      "Color" in {
        render[Typescript](declaration[Color]) shouldBe "export type Color = { red: number, green: number, blue: number };"
      }

      "Circle" in {
        render[Typescript](declaration[Circle]) shouldBe "export type Circle = { radius: number, color: Color };"
      }

      "Rectangle" in {
        render[Typescript](declaration[Rectangle]) shouldBe "export type Rectangle = { width: number, height: number, color: Color };"
      }

      "Shape" in {
        render[Typescript](declaration[Shape]) shouldBe """export type Shape = (({ type: "Circle" } & Circle) | ({ type: "Rectangle" } & Rectangle) | ({ type: "ShapeGroup" } & ShapeGroup));"""
      }

      "Alpha" in {
        render[Typescript](declaration[Alpha]) shouldBe "export type Alpha = { name: string, char: string, bool: boolean };"
      }

      "ArrayClass" in {
        render[Typescript](declaration[ArrayClass]) shouldBe """export type ArrayClass = { aList: Array<string>, optField: (number | null) };"""
      }

      "Numeric" in {
        render[Typescript](declaration[Numeric]) shouldBe """export type Numeric = { double: number, float: number, int: number };"""
      }

      "ClassOrObject" in {
        render[Typescript](declaration[ClassOrObject]) shouldBe """export type ClassOrObject = (({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject));"""
      }

      "NestedClassOrObject" in {
        render[Typescript](declaration[NestedClassOrObject]) shouldBe """export type NestedClassOrObject = (({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject));"""
      }

      "Navigation" in {
        render[Typescript](declaration[Navigation]) shouldBe """export type Navigation = (({ type: "Node" } & Node) | ({ type: "NodeList" } & NodeList));"""
      }

      "ClassUUID" in {
        render[Typescript](declaration[ClassUUID]) shouldBe """export type ClassUUID = { a: string };"""
      }

      "ExternalReferences" in {
        render[Typescript](declaration[ExternalReferences]) shouldBe """export type ExternalReferences = { color: Color, nav: Navigation };"""
      }

      "Custom" in {
        render[Typescript](customDeclaration) shouldBe """export type Message = (({ level: "error" } & ErrorMessage) | ({ level: "warning" } & WarningMessage));"""
      }

      "ObjectsOnly" in {
        render[Typescript](declaration[ObjectsOnly]) shouldBe """export type ObjectsOnly = (({ type: "ObjectOne" } & ObjectOne) | ({ type: "ObjectTwo" } & ObjectTwo));"""
      }
    }

    "flow" - {
      "Color" in {
        render[Flow](declaration[Color]) shouldBe "export type Color = { red: number, green: number, blue: number };"
      }

      "Circle" in {
        render[Flow](declaration[Circle]) shouldBe "export type Circle = { radius: number, color: Color };"
      }

      "Rectangle" in {
        render[Flow](declaration[Rectangle]) shouldBe "export type Rectangle = { width: number, height: number, color: Color };"
      }

      "Shape" in {
        render[Flow](declaration[Shape]) shouldBe """export type Shape = (({ type: "Circle" } & Circle) | ({ type: "Rectangle" } & Rectangle) | ({ type: "ShapeGroup" } & ShapeGroup));"""
      }

      "Alpha" in {
        render[Flow](declaration[Alpha]) shouldBe "export type Alpha = { name: string, char: string, bool: boolean };"
      }

      "ArrayClass" in {
        render[Flow](declaration[ArrayClass]) shouldBe """export type ArrayClass = { aList: Array<string>, optField: (number | null) };"""
      }

      "Numeric" in {
        render[Flow](declaration[Numeric]) shouldBe """export type Numeric = { double: number, float: number, int: number };"""
      }

      "ClassOrObject" in {
        render[Flow](declaration[ClassOrObject]) shouldBe """export type ClassOrObject = (({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject));"""
      }

      "NestedClassOrObject" in {
        render[Flow](declaration[NestedClassOrObject]) shouldBe """export type NestedClassOrObject = (({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject));"""
      }

      "Navigation" in {
        render[Flow](declaration[Navigation]) shouldBe """export type Navigation = (({ type: "Node" } & Node) | ({ type: "NodeList" } & NodeList));"""
      }

      "ClassUUID" in {
        render[Flow](declaration[ClassUUID]) shouldBe """export type ClassUUID = { a: string };"""
      }

      "ExternalReferences" in {
        render[Flow](declaration[ExternalReferences]) shouldBe """export type ExternalReferences = { color: Color, nav: Navigation };"""
      }

      "ObjectsOnly" in {
        render[Flow](declaration[ObjectsOnly]) shouldBe """export type ObjectsOnly = (({ type: "ObjectOne" } & ObjectOne) | ({ type: "ObjectTwo" } & ObjectTwo));"""
      }
    }

    "elm" - {
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
  }

}
