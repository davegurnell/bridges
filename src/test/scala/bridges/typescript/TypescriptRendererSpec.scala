package bridges.typescript

import bridges.SampleTypes._
import bridges.core.Type
import bridges.core.syntax._
import org.scalatest._

class TypescriptRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    Typescript.render(decl[Color]) shouldBe "export type Color = { red: number, green: number, blue: number };"
  }

  "Circle" in {
    Typescript.render(decl[Circle]) shouldBe "export type Circle = { radius: number, color: Color };"
  }

  "Rectangle" in {
    Typescript.render(decl[Rectangle]) shouldBe "export type Rectangle = { width: number, height: number, color: Color };"
  }

  "Shape" in {
    Typescript.render(decl[Shape]) shouldBe """export type Shape = (({ type: "Circle" } & Circle) | ({ type: "Rectangle" } & Rectangle) | ({ type: "ShapeGroup" } & ShapeGroup));"""
  }

  "Alpha" in {
    Typescript.render(decl[Alpha]) shouldBe "export type Alpha = { name: string, char: string, bool: boolean };"
  }

  "ArrayClass" in {
    Typescript.render(decl[ArrayClass]) shouldBe """export type ArrayClass = { aList: string[], optField: (number | null) };"""
  }

  "Numeric" in {
    Typescript.render(decl[Numeric]) shouldBe """export type Numeric = { double: number, float: number, int: number };"""
  }

  "ClassOrObject" in {
    Typescript.render(decl[ClassOrObject]) shouldBe """export type ClassOrObject = (({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject));"""
  }

  "NestedClassOrObject" in {
    Typescript.render(decl[NestedClassOrObject]) shouldBe """export type NestedClassOrObject = (({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject));"""
  }

  "Navigation" in {
    Typescript.render(decl[Navigation]) shouldBe """export type Navigation = (({ type: "Node" } & Node) | ({ type: "NodeList" } & NodeList));"""
  }

  "ClassUUID" in {
    Typescript.render(decl[ClassUUID]) shouldBe """export type ClassUUID = { a: UUID };"""
  }

  "ClassDate" in {
    Typescript.render(decl[ClassDate]) shouldBe """export type ClassDate = { a: Date };"""
  }

  "ExternalReferences" in {
    Typescript.render(decl[ExternalReferences]) shouldBe """export type ExternalReferences = { color: Color, nav: Navigation };"""
  }

  "Custom" in {
    val mappings: Map[Type, String] = Map(
      customErrorMsg.tpe   -> "{ level: \"error\" }",
      customWarningMsg.tpe -> "{ level: \"warning\" }"
    )
    Typescript.render(customDeclaration, mappings) shouldBe """export type Message = (({ level: "error" } & ErrorMessage) | ({ level: "warning" } & WarningMessage));"""
  }

  "ObjectsOnly" in {
    Typescript.render(decl[ObjectsOnly]) shouldBe """export type ObjectsOnly = (({ type: "ObjectOne" } & ObjectOne) | ({ type: "ObjectTwo" } & ObjectTwo));"""
  }
}
