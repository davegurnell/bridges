package bridges.typescript

import bridges.SampleTypes._
import bridges.typescript.TsType._
import bridges.typescript.syntax._
import org.scalatest._

class TsRendererSpec extends FreeSpec with Matchers {
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
    Typescript.render(decl[Shape]) shouldBe """export type Shape = { type: "Circle", radius: number, color: Color } | { type: "Rectangle", width: number, height: number, color: Color } | { type: "ShapeGroup", leftShape: Shape, rightShape: Shape };"""
  }

  "Alpha" in {
    Typescript.render(decl[Alpha]) shouldBe "export type Alpha = { name: string, char: string, bool: boolean };"
  }

  "ArrayClass" in {
    Typescript.render(decl[ArrayClass]) shouldBe """export type ArrayClass = { aList: string[], optField: number | null };"""
  }

  "Numeric" in {
    Typescript.render(decl[Numeric]) shouldBe """export type Numeric = { double: number, float: number, int: number };"""
  }

  "ClassOrObject" in {
    Typescript.render(decl[ClassOrObject]) shouldBe """export type ClassOrObject = { type: "MyClass", value: number } | { type: "MyObject" };"""
  }

  "NestedClassOrObject" in {
    Typescript.render(decl[NestedClassOrObject]) shouldBe """export type NestedClassOrObject = { type: "MyClass", value: number } | { type: "MyObject" };"""
  }

  "Navigation" in {
    Typescript.render(decl[Navigation]) shouldBe """export type Navigation = { type: "Node", name: string, children: Navigation[] } | { type: "NodeList", all: Navigation[] };"""
  }

  "ClassUUID" in {
    Typescript.render(decl[ClassUUID]) shouldBe """export type ClassUUID = { a: UUID };"""
  }

  "ClassDate" in {
    Typescript.render(decl[ClassDate]) shouldBe """export type ClassDate = { a: Date };"""
  }

  "Recursive" in {
    Typescript.render(decl[Recursive]) shouldBe """export type Recursive = { head: number, tail: Recursive | null };"""
  }

  "Recursive2" in {
    Typescript.render(decl[Recursive2]) shouldBe """export type Recursive2 = { head: number, tail: Recursive2[] };"""
  }

  "ExternalReferences" in {
    Typescript.render(decl[ExternalReferences]) shouldBe """export type ExternalReferences = { color: Color, nav: Navigation };"""
  }

  "ObjectsOnly" in {
    Typescript.render(decl[ObjectsOnly]) shouldBe """export type ObjectsOnly = { type: "ObjectOne" } | { type: "ObjectTwo" };"""
  }

  "Union of Union" in {
    Typescript.render("A" := Ref("B") | Ref("C") | Ref("D")) shouldBe """export type A = B | C | D;"""
  }

  "Inter of Inter" in {
    Typescript.render("A" := Ref("B") & Ref("C") & Ref("D")) shouldBe """export type A = B & C & D;"""
  }
}
