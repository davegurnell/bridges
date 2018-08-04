package bridges.flow

import bridges.SampleTypes._
import bridges.syntax._
import org.scalatest._

class FlowRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    Flow.render(declaration[Color]) shouldBe "export type Color = { red: number, green: number, blue: number };"
  }

  "Circle" in {
    Flow.render(declaration[Circle]) shouldBe "export type Circle = { radius: number, color: Color };"
  }

  "Rectangle" in {
    Flow.render(declaration[Rectangle]) shouldBe "export type Rectangle = { width: number, height: number, color: Color };"
  }

  "Shape" in {
    Flow.render(declaration[Shape]) shouldBe """export type Shape = ({ type: "Circle" } & Circle) | ({ type: "Rectangle" } & Rectangle) | ({ type: "ShapeGroup" } & ShapeGroup);"""
  }

  "Alpha" in {
    Flow.render(declaration[Alpha]) shouldBe "export type Alpha = { name: string, char: string, bool: boolean };"
  }

  "ArrayClass" in {
    Flow.render(declaration[ArrayClass]) shouldBe """export type ArrayClass = { aList: string[], optField: ?number };"""
  }

  "Numeric" in {
    Flow.render(declaration[Numeric]) shouldBe """export type Numeric = { double: number, float: number, int: number };"""
  }

  "ClassOrObject" in {
    Flow.render(declaration[ClassOrObject]) shouldBe """export type ClassOrObject = ({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject);"""
  }

  "NestedClassOrObject" in {
    Flow.render(declaration[NestedClassOrObject]) shouldBe """export type NestedClassOrObject = ({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject);"""
  }

  "Navigation" in {
    Flow.render(declaration[Navigation]) shouldBe """export type Navigation = ({ type: "Node" } & Node) | ({ type: "NodeList" } & NodeList);"""
  }

  "ClassUUID" in {
    Flow.render(declaration[ClassUUID]) shouldBe """export type ClassUUID = { a: UUID };"""
  }

  "ClassDate" in {
    Flow.render(declaration[ClassDate]) shouldBe """export type ClassDate = { a: Date };"""
  }

  "ExternalReferences" in {
    Flow.render(declaration[ExternalReferences]) shouldBe """export type ExternalReferences = { color: Color, nav: Navigation };"""
  }

  "ObjectsOnly" in {
    Flow.render(declaration[ObjectsOnly]) shouldBe """export type ObjectsOnly = ({ type: "ObjectOne" } & ObjectOne) | ({ type: "ObjectTwo" } & ObjectTwo);"""
  }
}
