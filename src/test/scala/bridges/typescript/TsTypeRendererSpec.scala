package bridges.typescript

import bridges.SampleTypes._
import bridges.core.DeclF
import bridges.typescript.TsType._
import bridges.typescript.syntax._
import org.scalatest._
import unindent._

class TsTypeRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    Typescript.render(decl[Color]) shouldBe {
      i"""
      export interface Color {
        red: number;
        green: number;
        blue: number;
      }
      """
    }
  }

  "Circle" in {
    Typescript.render(decl[Circle]) shouldBe {
      i"""
      export interface Circle {
        radius: number;
        color: Color;
      }
      """
    }
  }

  "Rectangle" in {
    Typescript.render(decl[Rectangle]) shouldBe {
      i"""
      export interface Rectangle {
        width: number;
        height: number;
        color: Color;
      }
      """
    }
  }

  "Shape" in {
    Typescript.render(decl[Shape]) shouldBe {
      i"""
      export type Shape = { type: "Circle", radius: number, color: Color } | { type: "Rectangle", width: number, height: number, color: Color } | { type: "ShapeGroup", leftShape: Shape, rightShape: Shape };
      """
    }
  }

  "Alpha" in {
    Typescript.render(decl[Alpha]) shouldBe {
      i"""
      export interface Alpha {
        name: string;
        char: string;
        bool: boolean;
      }
      """
    }
  }

  "ArrayClass" in {
    Typescript.render(decl[ArrayClass]) shouldBe {
      i"""
      export interface ArrayClass {
        aList: string[];
        optField: number | null;
      }
      """
    }
  }

  "Numeric" in {
    Typescript.render(decl[Numeric]) shouldBe {
      i"""
      export interface Numeric {
        double: number;
        float: number;
        int: number;
      }
      """
    }
  }

  "ClassOrObject" in {
    Typescript.render(decl[ClassOrObject]) shouldBe {
      i"""
      export type ClassOrObject = { type: "MyClass", value: number } | { type: "MyObject" };
      """
    }
  }

  "NestedClassOrObject" in {
    Typescript.render(decl[NestedClassOrObject]) shouldBe {
      i"""
      export type NestedClassOrObject = { type: "MyClass", value: number } | { type: "MyObject" };
      """
    }
  }

  "Navigation" in {
    Typescript.render(decl[Navigation]) shouldBe {
      i"""
      export type Navigation = { type: "Node", name: string, children: Navigation[] } | { type: "NodeList", all: Navigation[] };
      """
    }
  }

  "ClassUUID" in {
    Typescript.render(decl[ClassUUID]) shouldBe {
      i"""
      export interface ClassUUID {
        a: UUID;
      }
      """
    }
  }

  "ClassDate" in {
    Typescript.render(decl[ClassDate]) shouldBe {
      i"""
      export interface ClassDate {
        a: Date;
      }
      """
    }
  }

  "Recursive" in {
    Typescript.render(decl[Recursive]) shouldBe {
      i"""
      export interface Recursive {
        head: number;
        tail: Recursive | null;
      }
      """
    }
  }

  "Recursive2" in {
    Typescript.render(decl[Recursive2]) shouldBe {
      i"""
      export interface Recursive2 {
        head: number;
        tail: Recursive2[];
      }
      """
    }
  }

  "ExternalReferences" in {
    Typescript.render(decl[ExternalReferences]) shouldBe {
      i"""
      export interface ExternalReferences {
        color: Color;
        nav: Navigation;
      }
      """
    }
  }

  "ObjectsOnly" in {
    Typescript.render(decl[ObjectsOnly]) shouldBe {
      i"""
      export type ObjectsOnly = { type: "ObjectOne" } | { type: "ObjectTwo" };
      """
    }
  }

  "Union of Union" in {
    Typescript.render("A" := Ref("B") | Ref("C") | Ref("D")) shouldBe {
      i"""
      export type A = B | C | D;
      """
    }
  }

  "Inter of Inter" in {
    Typescript.render("A" := Ref("B") & Ref("C") & Ref("D")) shouldBe {
      i"""
      export type A = B & C & D;
      """
    }
  }

  "Generic Decl" in {
    Typescript.render(decl("Pair", "A", "B")(struct("a" -> Ref("A"), "b" -> Ref("B")))) shouldBe {
      i"""
      export interface Pair<A, B> {
        a: A;
        b: B;
      }
      """
    }
  }

  "Generic Ref" in {
    Typescript.render(decl("Numbers")(ref("Pair", Real, Real))) shouldBe {
      i"""
      export type Numbers = Pair<number, number>;
      """
    }
  }
}
