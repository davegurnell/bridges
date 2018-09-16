package bridges.typescript

import bridges.SampleTypes._
import bridges.syntax._
import org.scalatest._

class TypescriptRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    Typescript.render(declaration[Color]) shouldBe "export type Color = { red: number, green: number, blue: number };"
  }

  "Circle" in {
    Typescript.render(declaration[Circle]) shouldBe "export type Circle = { radius: number, color: Color };"
  }

  "Rectangle" in {
    Typescript.render(declaration[Rectangle]) shouldBe "export type Rectangle = { width: number, height: number, color: Color };"
  }

  "Shape" in {
    Typescript.render(declaration[Shape]) shouldBe """export type Shape = (({ type: "Circle" } & Circle) | ({ type: "Rectangle" } & Rectangle) | ({ type: "ShapeGroup" } & ShapeGroup));"""
  }

  "Alpha" in {
    Typescript.render(declaration[Alpha]) shouldBe "export type Alpha = { name: string, char: string, bool: boolean };"
  }

  "ArrayClass" in {
    Typescript.render(declaration[ArrayClass]) shouldBe """export type ArrayClass = { aList: string[], optField: (number | null) };"""
  }

  "Numeric" in {
    Typescript.render(declaration[Numeric]) shouldBe """export type Numeric = { double: number, float: number, int: number };"""
  }

  "ClassOrObject" in {
    Typescript.render(declaration[ClassOrObject]) shouldBe """export type ClassOrObject = (({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject));"""
  }

  "NestedClassOrObject" in {
    Typescript.render(declaration[NestedClassOrObject]) shouldBe """export type NestedClassOrObject = (({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject));"""
  }

  "Navigation" in {
    Typescript.render(declaration[Navigation]) shouldBe """export type Navigation = (({ type: "Node" } & Node) | ({ type: "NodeList" } & NodeList));"""
  }

  "ClassUUID" in {
    Typescript.render(declaration[ClassUUID]) shouldBe """export type ClassUUID = { a: UUID };"""
  }

  "ClassDate" in {
    Typescript.render(declaration[ClassDate]) shouldBe """export type ClassDate = { a: Date };"""
  }

  "ExternalReferences" in {
    Typescript.render(declaration[ExternalReferences]) shouldBe """export type ExternalReferences = { color: Color, nav: Navigation };"""
  }

  //TODO fix this
//  "Custom" in {
//    Typescript.render(customDeclaration) shouldBe """export type Message = (({ level: "error" } & ErrorMessage) | ({ level: "warning" } & WarningMessage));"""
//  }

  "ObjectsOnly" in {
    Typescript.render(declaration[ObjectsOnly]) shouldBe """export type ObjectsOnly = (({ type: "ObjectOne" } & ObjectOne) | ({ type: "ObjectTwo" } & ObjectTwo));"""
  }
}
