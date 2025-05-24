package bridges

import bridges.TsType._
import bridges.syntax._
import bridges.SampleTypes._
import munit.FunSuite
import unindent._

class TsGuardRendererSuite extends FunSuite {
  test("Color") {
    assertEquals(
      TypescriptGuard.render(decl[Color]),
      i"""
      export const isColor = (v: any): v is Color => {
        return typeof v === "object" && v != null && "red" in v && typeof v.red === "number" && "green" in v && typeof v.green === "number" && "blue" in v && typeof v.blue === "number";
      }
      """
    )
  }

  test("Circle") {
    assertEquals(
      TypescriptGuard.render(decl[Circle]),
      i"""
      export const isCircle = (v: any): v is Circle => {
        return typeof v === "object" && v != null && "radius" in v && typeof v.radius === "number" && "color" in v && isColor(v.color);
      }
      """
    )
  }

  test("Rectangle") {
    assertEquals(
      TypescriptGuard.render(decl[Rectangle]),
      i"""
      export const isRectangle = (v: any): v is Rectangle => {
        return typeof v === "object" && v != null && "width" in v && typeof v.width === "number" && "height" in v && typeof v.height === "number" && "color" in v && isColor(v.color);
      }
      """
    )
  }

  test("Shape") {
    assertEquals(
      TypescriptGuard.render(decl[Shape]),
      i"""
      export const isShape = (v: any): v is Shape => {
        return typeof v === "object" && v != null && "type" in v && (v.type === "Circle" && isCircle(v) || v.type === "Rectangle" && isRectangle(v) || v.type === "ShapeGroup" && isShapeGroup(v));
      }
      """
    )
  }

  test("Alpha") {
    assertEquals(
      TypescriptGuard.render(decl[Alpha]),
      i"""
      export const isAlpha = (v: any): v is Alpha => {
        return typeof v === "object" && v != null && "name" in v && typeof v.name === "string" && "char" in v && typeof v.char === "string" && "bool" in v && typeof v.bool === "boolean";
      }
      """
    )
  }

  test("ArrayClass") {
    assertEquals(
      TypescriptGuard.render(decl[ArrayClass]),
      i"""
      export const isArrayClass = (v: any): v is ArrayClass => {
        return typeof v === "object" && v != null && "aList" in v && Array.isArray(v.aList) && v.aList.every((i: any) => typeof i === "string") && (!("optField" in v) || typeof v.optField === "number" || v.optField === null);
      }
      """
    )
  }

  test("Numeric") {
    assertEquals(
      TypescriptGuard.render(decl[Numeric]),
      i"""
      export const isNumeric = (v: any): v is Numeric => {
        return typeof v === "object" && v != null && "double" in v && typeof v.double === "number" && "float" in v && typeof v.float === "number" && "int" in v && typeof v.int === "number";
      }
      """
    )
  }

  test("ClassOrObject") {
    assertEquals(
      TypescriptGuard.render(decl[ClassOrObject]),
      i"""
      export const isClassOrObject = (v: any): v is ClassOrObject => {
        return typeof v === "object" && v != null && "type" in v && (v.type === "MyClass" && isMyClass(v) || v.type === "MyObject" && isMyObject(v));
      }
      """
    )
  }

  test("NestedClassOrObject") {
    assertEquals(
      TypescriptGuard.render(decl[NestedClassOrObject]),
      i"""
      export const isNestedClassOrObject = (v: any): v is NestedClassOrObject => {
        return typeof v === "object" && v != null && "type" in v && (v.type === "MyClass" && isMyClass(v) || v.type === "MyObject" && isMyObject(v));
      }
      """
    )
  }

  test("Navigation") {
    assertEquals(
      TypescriptGuard.render(decl[Navigation]),
      i"""
      export const isNavigation = (v: any): v is Navigation => {
        return typeof v === "object" && v != null && "type" in v && (v.type === "NodeList" && isNodeList(v) || v.type === "Node" && isNode(v));
      }
      """
    )
  }

  test("ClassUUID") {
    assertEquals(
      TypescriptGuard.render(decl[ClassUUID]),
      i"""
      export const isClassUUID = (v: any): v is ClassUUID => {
        return typeof v === "object" && v != null && "a" in v && isUUID(v.a);
      }
      """
    )
  }

  test("ClassDate") {
    assertEquals(
      TypescriptGuard.render(decl[ClassDate]),
      i"""
      export const isClassDate = (v: any): v is ClassDate => {
        return typeof v === "object" && v != null && "a" in v && isDate(v.a);
      }
      """
    )
  }

  test("Recursive") {
    assertEquals(
      TypescriptGuard.render(decl[Recursive]),
      i"""
      export const isRecursive = (v: any): v is Recursive => {
        return typeof v === "object" && v != null && "head" in v && typeof v.head === "number" && (!("tail" in v) || isRecursive(v.tail) || v.tail === null);
      }
      """
    )
  }

  test("Recursive2") {
    assertEquals(
      TypescriptGuard.render(decl[Recursive2]),
      i"""
      export const isRecursive2 = (v: any): v is Recursive2 => {
        return typeof v === "object" && v != null && "head" in v && typeof v.head === "number" && "tail" in v && Array.isArray(v.tail) && v.tail.every((i: any) => isRecursive2(i));
      }
      """
    )
  }

  test("ExternalReferences") {
    assertEquals(
      TypescriptGuard.render(decl[ExternalReferences]),
      i"""
      export const isExternalReferences = (v: any): v is ExternalReferences => {
        return typeof v === "object" && v != null && "color" in v && isColor(v.color) && "nav" in v && isNavigation(v.nav);
      }
      """
    )
  }

  test("ObjectsOnly") {
    assertEquals(
      TypescriptGuard.render(decl[ObjectsOnly]),
      i"""
      export const isObjectsOnly = (v: any): v is ObjectsOnly => {
        return typeof v === "object" && v != null && "type" in v && (v.type === "ObjectOne" && isObjectOne(v) || v.type === "ObjectTwo" && isObjectTwo(v));
      }
      """
    )
  }

  test("Union of Union") {
    assertEquals(
      TypescriptGuard.render(decl("A")(Ref("B") | Ref("C") | Ref("D"))),
      i"""
      export const isA = (v: any): v is A => {
        return isB(v) || isC(v) || isD(v);
      }
      """
    )
  }

  test("Inter of Inter") {
    assertEquals(
      TypescriptGuard.render(decl("A")(Ref("B") & Ref("C") & Ref("D"))),
      i"""
      export const isA = (v: any): v is A => {
        return isB(v) && isC(v) && isD(v);
      }
      """
    )
  }

  test("Generic Decl") {
    assertEquals(
      TypescriptGuard.render(
        decl("Pair", "A", "B")(
          struct(
            "a" ---> Ref("A"),
            "b" --?> Ref("B")
          )
        )
      ),
      i"""
      export const isPair = <A, B>(isA: (a: any) => a is A, isB: (b: any) => b is B) => (v: any): v is Pair<A, B> => {
        return typeof v === "object" && v != null && "a" in v && isA(v.a) && (!("b" in v) || isB(v.b));
      }
      """
    )
  }

  test("Applications of Generics") {
    assertEquals(
      TypescriptGuard.render(decl("Cell")(ref("Pair", Str, Num))),
      i"""
      export const isCell = (v: any): v is Cell => {
        return isPair((a0: any): a0 is string => typeof a0 === "string", (a1: any): a1 is number => typeof a1 === "number")(v);
      }
      """
    )

    assertEquals(
      TypescriptGuard.render(decl("Same", "A")(ref("Pair", ref("A"), ref("A")))),
      i"""
      export const isSame = <A>(isA: (a: any) => a is A) => (v: any): v is Same<A> => {
        return isPair((a0: any): a0 is A => isA(a0), (a1: any): a1 is A => isA(a1))(v);
      }
      """
    )

    assertEquals(
      TypescriptGuard.render(decl("AnyPair")(ref("Pair", Any, Any))),
      i"""
      export const isAnyPair = (v: any): v is AnyPair => {
        return isPair((a0: any): a0 is any => true, (a1: any): a1 is any => true)(v);
      }
      """
    )
  }

  test("Numeric types") {
    assertEquals(
      TypescriptGuard.render(decl[NumericTypes]),
      i"""
      export const isNumericTypes = (v: any): v is NumericTypes => {
        return typeof v === "object" && v != null && "int" in v && typeof v.int === "number" && "long" in v && typeof v.long === "number" && "float" in v && typeof v.float === "number" && "double" in v && typeof v.double === "number" && "bigDecimal" in v && typeof v.bigDecimal === "number";
      }
      """
    )
  }

  test("Tuple") {
    assertEquals(
      TypescriptGuard.render(decl("Cell")(tuple(Str, Num))),
      i"""
      export const isCell = (v: any): v is Cell => {
        return Array.isArray(v) && v.length === 2 && typeof v[0] === "string" && typeof v[1] === "number";
      }
      """
    )
  }

  test("Empty tuple") {
    assertEquals(
      TypescriptGuard.render(decl("Empty")(tuple())),
      i"""
      export const isEmpty = (v: any): v is Empty => {
        return Array.isArray(v) && v.length === 0;
      }
      """
    )
  }

  test("Structs with rest fields") {
    assertEquals(
      TypescriptGuard.render(decl("Dict")(record(Str, Num))),
      i"""
      export const isDict = (v: any): v is Dict => {
        return typeof v === "object" && v != null && Object.keys(v).every((k: any) => typeof k === "string" && typeof v[k] === "number");
      }
      """
    )

    assertEquals(
      TypescriptGuard.render(
        decl("Dict")(
          struct(
            "a" ---> Str,
            "b" --?> Num
          ).withRest(Str, Bool, "c")
        )
      ),
      i"""
      export const isDict = (v: any): v is Dict => {
        return typeof v === "object" && v != null && "a" in v && typeof v.a === "string" && (!("b" in v) || typeof v.b === "number") && Object.keys(v).every((k: any) => ["a", "b"].includes(k) || typeof k === "string" && typeof v[k] === "boolean");
      }
      """
    )
  }

  test("Function types") {
    assertEquals(
      TypescriptGuard.render(
        decl("Rule")(
          struct(
            "message" ---> Str,
            "apply"   ---> func("value" -> Unknown)(Bool)
          )
        )
      ),
      i"""
      export const isRule = (v: any): v is Rule => {
        return typeof v === "object" && v != null && "message" in v && typeof v.message === "string" && "apply" in v && typeof v.apply === "function";
      }
      """
    )

    assertEquals(
      TypescriptGuard.render(
        decl("Funcy")(
          tuple(
            func("arg" -> tuple(Str))(tuple(Str)),
            func("arg" -> tuple(Num))(tuple(Num))
          )
        )
      ),
      i"""
      export const isFuncy = (v: any): v is Funcy => {
        return Array.isArray(v) && v.length === 2 && typeof v[0] === "function" && typeof v[1] === "function";
      }
      """
    )
  }
}
