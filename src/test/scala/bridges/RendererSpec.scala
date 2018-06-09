package bridges

import org.scalatest._
import syntax._
import SampleTypes._
import bridges.Type.Str

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

      "Navigation" in {
        render[Typescript](declaration[Navigation]) shouldBe """export type Navigation = (({ type: "Node" } & Node) | ({ type: "NodeList" } & NodeList));"""
      }

      "Custom" in {
        render[Typescript](customDeclaration) shouldBe """export type Message = (({ level: "error" } & ErrorMessage) | ({ level: "warning" } & WarningMessage));"""
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

      "Navigation" in {
        render[Flow](declaration[Navigation]) shouldBe """export type Navigation = (({ type: "Node" } & Node) | ({ type: "NodeList" } & NodeList));"""
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

      "Navigation" in {
        render[Elm](declaration[Navigation]) shouldBe """type Navigation = Node String (List Navigation) | NodeList (List Navigation)"""
      }

      "MyUUID" in {
        // we want to treat UUID as string, using an override
        implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
          Encoder.pure(Str)

        render[Elm](declaration[MyUUID]) shouldBe """type alias MyUUID = { uuid: String }"""
      }

    }
  }

}
