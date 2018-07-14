package bridges.typescript

import bridges.SampleTypes._
import bridges.core._
import bridges.core.Type._
import bridges.syntax._
import shapeless.Typeable
import org.scalatest._

class TypescriptRendererSpec extends FreeSpec with Matchers {
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
    render[Typescript](declaration[ArrayClass]) shouldBe """export type ArrayClass = { aList: string[], optField: (number | null) };"""
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
