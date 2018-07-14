package bridges.flow

import bridges.SampleTypes._
import bridges.core._
import bridges.core.Type._
import bridges.syntax._
import shapeless.Typeable
import org.scalatest._

class FlowRendererSpec extends FreeSpec with Matchers {
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
    render[Flow](declaration[ArrayClass]) shouldBe """export type ArrayClass = { aList: string[], optField: ?number };"""
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
