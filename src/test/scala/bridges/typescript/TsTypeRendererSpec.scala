package bridges.typescript

import munit.FunSuite
import unindent.*

class TsTypeRendererSpec extends FunSuite:
  import SampleTypes.*
  import TsType.*
  import syntax.*

  test("Color") {
    assertEquals(
      Typescript.render(decl[Color]),
      i"""
      export interface Color {
        red: number;
        green: number;
        blue: number;
      }
      """
    )
  }

  test("Circle") {
    assertEquals(
      Typescript.render(decl[Circle]),
      i"""
      export interface Circle {
        radius: number;
        color: Color;
      }
      """
    )
  }

  test("Rectangle") {
    assertEquals(
      Typescript.render(decl[Rectangle]),
      i"""
      export interface Rectangle {
        width: number;
        height: number;
        color: Color;
      }
      """
    )
  }

  test("Shape") {
    assertEquals(
      Typescript.render(decl[Shape]),
      i"""
      export type Shape = { type: "Circle", radius: number, color: Color } | { type: "Rectangle", width: number, height: number, color: Color } | { type: "ShapeGroup", leftShape: Shape, rightShape: Shape };
      """
    )
  }

  test("Alpha") {
    assertEquals(
      Typescript.render(decl[Alpha]),
      i"""
      export interface Alpha {
        name: string;
        char: string;
        bool: boolean;
      }
      """
    )
  }

  test("ArrayClass") {
    assertEquals(
      Typescript.render(decl[ArrayClass]),
      i"""
      export interface ArrayClass {
        aList: string[];
        optField?: number | null;
      }
      """
    )
  }

  test("Numeric") {
    assertEquals(
      Typescript.render(decl[Numeric]),
      i"""
      export interface Numeric {
        double: number;
        float: number;
        int: number;
      }
      """
    )
  }

  test("Singleton object") {
    assertEquals(
      Typescript.render(decl[MyObject.type]),
      i"""
      export interface MyObject {
      }
      """
    )
  }

  test("ClassOrObject") {
    assertEquals(
      Typescript.render(decl[ClassOrObject]),
      i"""
      export type ClassOrObject = { type: "MyClass", value: number } | { type: "MyObject" };
      """
    )
  }

  test("NestedClassOrObject") {
    assertEquals(
      Typescript.render(decl[NestedClassOrObject]),
      i"""
      export type NestedClassOrObject = { type: "MyClass", value: number } | { type: "MyObject" };
      """
    )
  }

  test("Navigation") {
    assertEquals(
      Typescript.render(decl[Navigation]),
      i"""
      export type Navigation = { type: "Node", name: string, children: Navigation[] } | { type: "NodeList", all: Navigation[] };
      """
    )
  }

  test("ClassUUID") {
    assertEquals(
      Typescript.render(decl[ClassUUID]),
      i"""
      export interface ClassUUID {
        a: UUID;
      }
      """
    )
  }

  test("ClassDate") {
    assertEquals(
      Typescript.render(decl[ClassDate]),
      i"""
      export interface ClassDate {
        a: Date;
      }
      """
    )
  }

  test("Recursive") {
    assertEquals(
      Typescript.render(decl[Recursive]),
      i"""
      export interface Recursive {
        head: number;
        tail?: Recursive | null;
      }
      """
    )
  }

  test("Recursive2") {
    assertEquals(
      Typescript.render(decl[Recursive2]),
      i"""
      export interface Recursive2 {
        head: number;
        tail: Recursive2[];
      }
      """
    )
  }

  test("ExternalReferences") {
    assertEquals(
      Typescript.render(decl[ExternalReferences]),
      i"""
      export interface ExternalReferences {
        color: Color;
        nav: Navigation;
      }
      """
    )
  }

  test("ObjectsOnly") {
    assertEquals(
      Typescript.render(decl[ObjectsOnly]),
      i"""
      export type ObjectsOnly = { type: "ObjectOne" } | { type: "ObjectTwo" };
      """
    )
  }

  test("Union of Union") {
    assertEquals(
      Typescript.render(decl("A")(union(Ref("B"), Ref("C"), Ref("D")))),
      i"""
      export type A = B | C | D;
      """
    )
  }

  test("Inter of Inter") {
    assertEquals(
      Typescript.render(decl("A")(intersect(Ref("B"), Ref("C"), Ref("D")))),
      i"""
      export type A = B & C & D;
      """
    )
  }

  test("Generic Decl") {
    assertEquals(
      Typescript.render(
        decl("Pair", "A", "B")(
          struct(
            "a" --> Ref("A"),
            "b" -?> Ref("B")
          )
        )
      ),
      i"""
      export interface Pair<A, B> {
        a: A;
        b?: B;
      }
      """
    )
  }

  test("Applications of Generics") {
    assertEquals(
      Typescript.render(decl("Cell")(ref("Pair", Str, Intr))),
      i"""
      export type Cell = Pair<string, number>;
      """
    )

    assertEquals(
      Typescript.render(decl("Same", "A")(ref("Pair", ref("A"), ref("A")))),
      i"""
      export type Same<A> = Pair<A, A>;
      """
    )

    assertEquals(
      Typescript.render(decl("AnyPair")(ref("Pair", Any, Any))),
      i"""
      export type AnyPair = Pair<any, any>;
      """
    )
  }

  test("Numeric types") {
    assertEquals(
      Typescript.render(decl[NumericTypes]),
      i"""
      export interface NumericTypes {
        int: number;
        long: number;
        float: number;
        double: number;
        bigDecimal: number;
      }
      """
    )
  }

  test("Tuple") {
    assertEquals(
      Typescript.render(decl("Cell")(tuple(Str, Intr))),
      i"""
      export type Cell = [string, number];
      """
    )
  }

  test("Empty tuple") {
    assertEquals(
      Typescript.render(decl("Empty")(tuple())),
      i"""
      export type Empty = [];
      """
    )
  }

  test("Structs with rest fields") {
    assertEquals(
      Typescript.render(decl("Dict")(dict(Str, Intr))),
      i"""
      export interface Dict {
        [key: string]: number;
      }
      """
    )

    assertEquals(
      Typescript.render(
        decl("Dict")(
          struct(
            "a" --> Str,
            "b" -?> Intr
          ).withRest(Str, Bool, "c")
        )
      ),
      i"""
      export interface Dict {
        a: string;
        b?: number;
        [c: string]: boolean;
      }
      """
    )
  }

  test("Unknown and any") {
    assertEquals(
      Typescript.render(decl("UnknownAndAny")(struct("foo" --> Any, "bar" --> Unknown))),
      i"""
      export interface UnknownAndAny {
        foo: any;
        bar: unknown;
      }
      """
    )
  }

  test("Function types") {
    assertEquals(
      Typescript.render(
        decl("Rule")(
          struct(
            "message" --> Str,
            "apply" --> func("value" -> Unknown)(Bool)
          )
        )
      ),
      i"""
      export interface Rule {
        message: string;
        apply: (value: unknown) => boolean;
      }
      """
    )

    assertEquals(
      Typescript.render(decl("Funcy")(tuple(func("arg" -> tuple(Str))(tuple(Str)), func("arg" -> tuple(Intr))(tuple(Intr))))),
      i"""
      export type Funcy = [(arg: [string]) => [string], (arg: [number]) => [number]];
      """
    )
  }
