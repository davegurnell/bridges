package bridges.typescript

import bridges.SampleTypes._
import bridges.typescript.TsType._
import bridges.typescript.syntax._
import org.scalatest._
import unindent._

class TsRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    TsTypeRenderer.render(decl[Color]) shouldBe {
      i"""
      export type Color = { red: number, green: number, blue: number };
      """
    }
  }

  "Circle" in {
    TsTypeRenderer.render(decl[Circle]) shouldBe {
      i"""
      export type Circle = { radius: number, color: Color };
      """
    }
  }

  "Rectangle" in {
    TsTypeRenderer.render(decl[Rectangle]) shouldBe {
      i"""
      export type Rectangle = { width: number, height: number, color: Color };
      """
    }
  }

  "Shape" in {
    TsTypeRenderer.render(decl[Shape]) shouldBe {
      i"""
      export type Shape = { type: "Circle", radius: number, color: Color } | { type: "Rectangle", width: number, height: number, color: Color } | { type: "ShapeGroup", leftShape: Shape, rightShape: Shape };
      """
    }
  }

  "Alpha" in {
    TsTypeRenderer.render(decl[Alpha]) shouldBe {
      i"""
      export type Alpha = { name: string, char: string, bool: boolean };
      """
    }
  }

  "ArrayClass" in {
    TsTypeRenderer.render(decl[ArrayClass]) shouldBe {
      i"""
      export type ArrayClass = { aList: string[], optField: number | null };
      """
    }
  }

  "Numeric" in {
    TsTypeRenderer.render(decl[Numeric]) shouldBe {
      i"""
      export type Numeric = { double: number, float: number, int: number };
      """
    }
  }

  "ClassOrObject" in {
    TsTypeRenderer.render(decl[ClassOrObject]) shouldBe {
      i"""
      export type ClassOrObject = { type: "MyClass", value: number } | { type: "MyObject" };
      """
    }
  }

  "NestedClassOrObject" in {
    TsTypeRenderer.render(decl[NestedClassOrObject]) shouldBe {
      i"""
      export type NestedClassOrObject = { type: "MyClass", value: number } | { type: "MyObject" };
      """
    }
  }

  "Navigation" in {
    TsTypeRenderer.render(decl[Navigation]) shouldBe {
      i"""
      export type Navigation = { type: "Node", name: string, children: Navigation[] } | { type: "NodeList", all: Navigation[] };
      """
    }
  }

  "ClassUUID" in {
    TsTypeRenderer.render(decl[ClassUUID]) shouldBe {
      i"""
      export type ClassUUID = { a: UUID };
      """
    }
  }

  "ClassDate" in {
    TsTypeRenderer.render(decl[ClassDate]) shouldBe {
      i"""
      export type ClassDate = { a: Date };
      """
    }
  }

  "Recursive" in {
    TsTypeRenderer.render(decl[Recursive]) shouldBe {
      i"""
      export type Recursive = { head: number, tail: Recursive | null };
      """
    }
  }

  "Recursive2" in {
    TsTypeRenderer.render(decl[Recursive2]) shouldBe {
      i"""
      export type Recursive2 = { head: number, tail: Recursive2[] };
      """
    }
  }

  "ExternalReferences" in {
    TsTypeRenderer.render(decl[ExternalReferences]) shouldBe {
      i"""
      export type ExternalReferences = { color: Color, nav: Navigation };
      """
    }
  }

  "ObjectsOnly" in {
    TsTypeRenderer.render(decl[ObjectsOnly]) shouldBe {
      i"""
      export type ObjectsOnly = { type: "ObjectOne" } | { type: "ObjectTwo" };
      """
    }
  }

  "Union of Union" in {
    TsTypeRenderer.render("A" := Ref("B") | Ref("C") | Ref("D")) shouldBe {
      i"""
      export type A = B | C | D;
      """
    }
  }

  "Inter of Inter" in {
    TsTypeRenderer.render("A" := Ref("B") & Ref("C") & Ref("D")) shouldBe {
      i"""
      export type A = B & C & D;
      """
    }
  }
}
