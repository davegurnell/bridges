package bridges.flow

import bridges.SampleTypes._
import bridges.core.Type._
import bridges.core.syntax._
import org.scalatest._

class FlowRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    Flow.render(decl[Color]) shouldBe "export type Color = { red: number, green: number, blue: number };"
  }

  "Circle" in {
    Flow.render(decl[Circle]) shouldBe "export type Circle = { radius: number, color: Color };"
  }

  "Rectangle" in {
    Flow.render(decl[Rectangle]) shouldBe "export type Rectangle = { width: number, height: number, color: Color };"
  }

  "Shape" in {
    Flow.render(decl[Shape]) shouldBe """export type Shape = ({ type: "Circle" } & Circle) | ({ type: "Rectangle" } & Rectangle) | ({ type: "ShapeGroup" } & ShapeGroup);"""
  }

  "Alpha" in {
    Flow.render(decl[Alpha]) shouldBe "export type Alpha = { name: string, char: string, bool: boolean };"
  }

  "ArrayClass" in {
    Flow.render(decl[ArrayClass]) shouldBe """export type ArrayClass = { aList: string[], optField: ?number };"""
  }

  "Numeric" in {
    Flow.render(decl[Numeric]) shouldBe """export type Numeric = { double: number, float: number, int: number };"""
  }

  "ClassOrObject" in {
    Flow.render(decl[ClassOrObject]) shouldBe """export type ClassOrObject = ({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject);"""
  }

  "NestedClassOrObject" in {
    Flow.render(decl[NestedClassOrObject]) shouldBe """export type NestedClassOrObject = ({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject);"""
  }

  "Navigation" in {
    Flow.render(decl[Navigation]) shouldBe """export type Navigation = ({ type: "Node" } & Node) | ({ type: "NodeList" } & NodeList);"""
  }

  "ClassUUID" in {
    Flow.render(decl[ClassUUID]) shouldBe """export type ClassUUID = { a: UUID };"""
  }

  "ClassDate" in {
    Flow.render(decl[ClassDate]) shouldBe """export type ClassDate = { a: Date };"""
  }

  "ExternalReferences" in {
    Flow.render(decl[ExternalReferences]) shouldBe """export type ExternalReferences = { color: Color, nav: Navigation };"""
  }

  "ObjectsOnly" in {
    Flow.render(decl[ObjectsOnly]) shouldBe """export type ObjectsOnly = ({ type: "ObjectOne" } & ObjectOne) | ({ type: "ObjectTwo" } & ObjectTwo);"""
  }

  "Optional of Array" in {
    Flow.render("Foo" := Opt(Arr(Str))) shouldBe """export type Foo = ?string[];"""
  }

  "Array of Optional" in {
    Flow.render("Foo" := Arr(Opt(Str))) shouldBe """export type Foo = (?string)[];"""
  }
}
