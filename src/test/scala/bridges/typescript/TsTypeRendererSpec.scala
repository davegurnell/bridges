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
        optField?: number | null;
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
        tail?: Recursive | null;
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
    Typescript.render(
      decl("Pair", "A", "B")(
        struct(
          "a" --> Ref("A"),
          "b" -?> Ref("B")
        )
      )
    ) shouldBe {
      i"""
      export interface Pair<A, B> {
        a: A;
        b?: B;
      }
      """
    }
  }

  "Applications of Generics" in {
    Typescript.render(decl("Cell")(ref("Pair", Str, Intr))) shouldBe {
      i"""
      export type Cell = Pair<string, number>;
      """
    }

    Typescript.render(decl("Same", "A")(ref("Pair", ref("A"), ref("A")))) shouldBe {
      i"""
      export type Same<A> = Pair<A, A>;
      """
    }

    Typescript.render(decl("AnyPair")(ref("Pair", Any, Any))) shouldBe {
      i"""
      export type AnyPair = Pair<any, any>;
      """
    }
  }

  "Numeric types" in {
    Typescript.render(decl[NumericTypes]) shouldBe {
      i"""
      export interface NumericTypes {
        int: number;
        long: number;
        float: number;
        double: number;
        bigDecimal: number;
      }
      """
    }
  }

  "Tuple" in {
    Typescript.render(decl("Cell")(tuple(Str, Intr))) shouldBe {
      i"""
      export type Cell = [string, number];
      """
    }
  }

  "Empty tuple" in {
    Typescript.render(decl("Empty")(tuple())) shouldBe {
      i"""
      export type Empty = [];
      """
    }
  }
}
