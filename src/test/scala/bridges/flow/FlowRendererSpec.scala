package bridges.flow

import bridges.SampleTypes._
import bridges.core.DeclF
import bridges.flow.FlowType._
import bridges.flow.syntax._
import org.scalatest._
import unindent._

class FlowRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    Flow.render(decl[Color]) shouldBe {
      i"""
      export type Color = { red: number, green: number, blue: number };
      """
    }
  }

  "Circle" in {
    Flow.render(decl[Circle]) shouldBe {
      i"""
      export type Circle = { radius: number, color: Color };
      """
    }
  }

  "Rectangle" in {
    Flow.render(decl[Rectangle]) shouldBe {
      i"""
      export type Rectangle = { width: number, height: number, color: Color };
      """
    }
  }

  "Shape" in {
    Flow.render(decl[Shape]) shouldBe {
      i"""
      export type Shape = { type: "Circle", radius: number, color: Color } | { type: "Rectangle", width: number, height: number, color: Color } | { type: "ShapeGroup", leftShape: Shape, rightShape: Shape };
      """
    }
  }

  "Alpha" in {
    Flow.render(decl[Alpha]) shouldBe {
      i"""
      export type Alpha = { name: string, char: string, bool: boolean };
      """
    }
  }

  "ArrayClass" in {
    Flow.render(decl[ArrayClass]) shouldBe {
      i"""
      export type ArrayClass = { aList: string[], optField: ?number };
      """
    }
  }

  "Numeric" in {
    Flow.render(decl[Numeric]) shouldBe {
      i"""
      export type Numeric = { double: number, float: number, int: number };
      """
    }
  }

  "ClassOrObject" in {
    Flow.render(decl[ClassOrObject]) shouldBe {
      i"""
      export type ClassOrObject = { type: "MyClass", value: number } | { type: "MyObject" };
      """
    }
  }

  "NestedClassOrObject" in {
    Flow.render(decl[NestedClassOrObject]) shouldBe {
      i"""
      export type NestedClassOrObject = { type: "MyClass", value: number } | { type: "MyObject" };
      """
    }
  }

  "Navigation" in {
    Flow.render(decl[Navigation]) shouldBe {
      i"""
      export type Navigation = { type: "Node", name: string, children: Navigation[] } | { type: "NodeList", all: Navigation[] };
      """
    }
  }

  "ClassUUID" in {
    Flow.render(decl[ClassUUID]) shouldBe {
      i"""
      export type ClassUUID = { a: UUID };
      """
    }
  }

  "ClassDate" in {
    Flow.render(decl[ClassDate]) shouldBe {
      i"""
      export type ClassDate = { a: Date };
      """
    }
  }

  "ExternalReferences" in {
    Flow.render(decl[ExternalReferences]) shouldBe {
      i"""
      export type ExternalReferences = { color: Color, nav: Navigation };
      """
    }
  }

  "Recursive" in {
    Flow.render(decl[Recursive]) shouldBe {
      i"""
      export type Recursive = { head: number, tail: ?Recursive };
      """
    }
  }

  "Recursive2" in {
    Flow.render(decl[Recursive2]) shouldBe {
      i"""
      export type Recursive2 = { head: number, tail: Recursive2[] };
      """
    }
  }

  "ObjectsOnly" in {
    Flow.render(decl[ObjectsOnly]) shouldBe {
      i"""
      export type ObjectsOnly = { type: "ObjectOne" } | { type: "ObjectTwo" };
      """
    }
  }

  "Optional of Array" in {
    Flow.render("Foo" := Arr(Str).?) shouldBe {
      i"""
      export type Foo = ?string[];
      """
    }
  }

  "Array of Optional" in {
    Flow.render("Foo" := Arr(Str.?)) shouldBe {
      i"""
      export type Foo = (?string)[];
      """
    }
  }

  "Union of Union" in {
    Flow.render("A" := Ref("B") | Ref("C") | Ref("D")) shouldBe {
      i"""
      export type A = B | C | D;
      """
    }
  }

  "Inter of Inter" in {
    Flow.render("A" := Ref("B") & Ref("C") & Ref("D")) shouldBe {
      i"""
      export type A = B & C & D;
      """
    }
  }

  "Generic Decl" in {
    Flow.render(decl("Pair", "A", "B")(struct("a" -> Ref("A"), "b" -> Ref("B")))) shouldBe {
      i"""
      export type Pair<A, B> = { a: A, b: B };
      """
    }
  }

  "Generic Ref" in {
    Flow.render(decl("Numbers")(ref("Pair", Real, Real))) shouldBe {
      i"""
      export type Numbers = Pair<number, number>;
      """
    }
  }

  "Numeric types" in {
    Flow.render(decl[NumericTypes]) shouldBe {
      i"""
      export type NumericTypes = { int: number, long: number, float: number, double: number, bigDecimal: number };
      """
    }
  }

  "Tuple" in {
    Flow.render(decl("Cell")(tuple(Str, Intr))) shouldBe {
      i"""
      export type Cell = [string, number];
      """
    }
  }

  "Empty tuple" in {
    Flow.render(decl("Empty")(tuple())) shouldBe {
      i"""
      export type Empty = [];
      """
    }
  }
}
