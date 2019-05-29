package bridges.typescript

import bridges.SampleTypes._
import bridges.typescript.TsType._
import bridges.typescript.syntax._
import org.scalatest._
import unindent._

class TypescriptGuardSpec extends FreeSpec with Matchers {
  "Color" in {
    TypescriptGuard.render(decl[Color]) shouldBe {
      i"""
      export const isColor = (v: any): boolean => {
        return typeof v.red === "number" && typeof v.green === "number" && typeof v.blue === "number";
      }

      export const asColor = (v: any): Color => {
        if(isColor(v)) {
          return v as Color;
        } else {
          throw new Error("Expected Color, received " + JSON.stringify(v, null, 2));
        }
      }
      """
    }

    TypescriptGuard.renderPred(decl[Color]) shouldBe {
      i"""
      export const isColor = (v: any): boolean => {
        return typeof v.red === "number" && typeof v.green === "number" && typeof v.blue === "number";
      }
      """
    }
  }

  "Circle" in {
    TypescriptGuard.renderPred(decl[Circle]) shouldBe {
      i"""
      export const isCircle = (v: any): boolean => {
        return typeof v.radius === "number" && isColor(v.color);
      }
      """
    }
  }

  "Rectangle" in {
    TypescriptGuard.renderPred(decl[Rectangle]) shouldBe {
      i"""
      export const isRectangle = (v: any): boolean => {
        return typeof v.width === "number" && typeof v.height === "number" && isColor(v.color);
      }
      """
    }
  }

  "Shape" in {
    TypescriptGuard.renderPred(decl[Shape]) shouldBe {
      i"""
      export const isShape = (v: any): boolean => {
        return v.type === "Circle" ? typeof v.radius === "number" && isColor(v.color) : v.type === "Rectangle" ? typeof v.width === "number" && typeof v.height === "number" && isColor(v.color) : v.type === "ShapeGroup" ? isShape(v.leftShape) && isShape(v.rightShape) : false;
      }
      """
    }
  }

  "Alpha" in {
    TypescriptGuard.renderPred(decl[Alpha]) shouldBe {
      i"""
      export const isAlpha = (v: any): boolean => {
        return typeof v.name === "string" && typeof v.char === "string" && typeof v.bool === "boolean";
      }
      """
    }
  }

  "ArrayClass" in {
    TypescriptGuard.renderPred(decl[ArrayClass]) shouldBe {
      i"""
      export const isArrayClass = (v: any): boolean => {
        return Array.isArray(v.aList) && v.aList.map((i: any) => (typeof i === "string")).reduce((a: any, b: any) => (a && b)) && (typeof v.optField === "number" || v.optField === null);
      }
      """
    }
  }

  "Numeric" in {
    TypescriptGuard.renderPred(decl[Numeric]) shouldBe {
      i"""
      export const isNumeric = (v: any): boolean => {
        return typeof v.double === "number" && typeof v.float === "number" && typeof v.int === "number";
      }
      """
    }
  }

  "ClassOrObject" in {
    TypescriptGuard.renderPred(decl[ClassOrObject]) shouldBe {
      i"""
      export const isClassOrObject = (v: any): boolean => {
        return v.type === "MyClass" ? typeof v.value === "number" : v.type === "MyObject" ? true : false;
      }
      """
    }
  }

  "NestedClassOrObject" in {
    TypescriptGuard.renderPred(decl[NestedClassOrObject]) shouldBe {
      i"""
      export const isNestedClassOrObject = (v: any): boolean => {
        return v.type === "MyClass" ? typeof v.value === "number" : v.type === "MyObject" ? true : false;
      }
      """
    }
  }

  "Navigation" in {
    TypescriptGuard.renderPred(decl[Navigation]) shouldBe {
      i"""
      export const isNavigation = (v: any): boolean => {
        return v.type === "Node" ? typeof v.name === "string" && Array.isArray(v.children) && v.children.map((i: any) => isNavigation(i)).reduce((a: any, b: any) => (a && b)) : v.type === "NodeList" ? Array.isArray(v.all) && v.all.map((i: any) => isNavigation(i)).reduce((a: any, b: any) => (a && b)) : false;
      }
      """
    }
  }

  "ClassUUID" in {
    TypescriptGuard.renderPred(decl[ClassUUID]) shouldBe {
      i"""
      export const isClassUUID = (v: any): boolean => {
        return isUUID(v.a);
      }
      """
    }
  }

  "ClassDate" in {
    TypescriptGuard.renderPred(decl[ClassDate]) shouldBe {
      i"""
      export const isClassDate = (v: any): boolean => {
        return isDate(v.a);
      }
      """
    }
  }

  "Recursive" in {
    TypescriptGuard.renderPred(decl[Recursive]) shouldBe {
      i"""
      export const isRecursive = (v: any): boolean => {
        return typeof v.head === "number" && (isRecursive(v.tail) || v.tail === null);
      }
      """
    }
  }

  "Recursive2" in {
    TypescriptGuard.renderPred(decl[Recursive2]) shouldBe {
      i"""
      export const isRecursive2 = (v: any): boolean => {
        return typeof v.head === "number" && Array.isArray(v.tail) && v.tail.map((i: any) => isRecursive2(i)).reduce((a: any, b: any) => (a && b));
      }
      """
    }
  }

  "ExternalReferences" in {
    TypescriptGuard.renderPred(decl[ExternalReferences]) shouldBe {
      i"""
      export const isExternalReferences = (v: any): boolean => {
        return isColor(v.color) && isNavigation(v.nav);
      }
      """
    }
  }

  "ObjectsOnly" in {
    TypescriptGuard.renderPred(decl[ObjectsOnly]) shouldBe {
      i"""
      export const isObjectsOnly = (v: any): boolean => {
        return v.type === "ObjectOne" ? true : v.type === "ObjectTwo" ? true : false;
      }
      """
    }
  }

  "Union of Union" in {
    TypescriptGuard.renderPred("A" := Ref("B") | Ref("C") | Ref("D")) shouldBe {
      i"""
      export const isA = (v: any): boolean => {
        return isB(v) || isC(v) || isD(v);
      }
      """
    }
  }

  "Inter of Inter" in {
    TypescriptGuard.renderPred("A" := Ref("B") & Ref("C") & Ref("D")) shouldBe {
      i"""
      export const isA = (v: any): boolean => {
        return isB(v) && isC(v) && isD(v);
      }
      """
    }
  }

  "Generic Decl" in {
    TypescriptGuard.render(decl("Pair", "A", "B")(struct("a" -> Ref("A"), "b" -> Ref("B")))) shouldBe {
      i"""
      export const isPair = (isA: (v: any) => A, isB: (v: any) => B) => (v: any): boolean => {
        return isA(v.a) && isB(v.b);
      }

      export const asPair = (isA: (v: any) => A, isB: (v: any) => B) => (v: any): Pair => {
        if(isPair(isA, isB)(v)) {
          return v as Pair;
        } else {
          throw new Error("Expected Pair, received " + JSON.stringify(v, null, 2));
        }
      }
      """
    }
  }

}
