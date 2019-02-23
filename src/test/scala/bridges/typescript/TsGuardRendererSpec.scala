package bridges.typescript

import bridges.SampleTypes._
import bridges.typescript.TsType._
import bridges.typescript.syntax._
import org.scalatest._
import unindent._

class TsGuardRendererSpec extends FreeSpec with Matchers {
  "Color" in {
    TsGuardRenderer.render(decl[Color]) shouldBe {
      i"""
      export function isColor(v: any): boolean {
        return typeof v.red === "number" && typeof v.green === "number" && typeof v.blue === "number";
      }

      export function asColor(v: any): Color {
        if(isColor(v)) {
          return v as Color;
        } else {
          throw new Error("Expected Color, received " + JSON.stringify(v, null, 2));
        }
      }
      """
    }

    TsGuardRenderer.renderPred(decl[Color]) shouldBe {
      i"""
      export function isColor(v: any): boolean {
        return typeof v.red === "number" && typeof v.green === "number" && typeof v.blue === "number";
      }
      """
    }
  }

  "Circle" in {
    TsGuardRenderer.renderPred(decl[Circle]) shouldBe {
      i"""
      export function isCircle(v: any): boolean {
        return typeof v.radius === "number" && isColor(v.color);
      }
      """
    }
  }

  "Rectangle" in {
    TsGuardRenderer.renderPred(decl[Rectangle]) shouldBe {
      i"""
      export function isRectangle(v: any): boolean {
        return typeof v.width === "number" && typeof v.height === "number" && isColor(v.color);
      }
      """
    }
  }

  "Shape" in {
    TsGuardRenderer.renderPred(decl[Shape]) shouldBe {
      i"""
      export function isShape(v: any): boolean {
        return v.type === "Circle" ? typeof v.radius === "number" && isColor(v.color) : v.type === "Rectangle" ? typeof v.width === "number" && typeof v.height === "number" && isColor(v.color) : v.type === "ShapeGroup" ? isShape(v.leftShape) && isShape(v.rightShape) : false;
      }
      """
    }
  }

  "Alpha" in {
    TsGuardRenderer.renderPred(decl[Alpha]) shouldBe {
      i"""
      export function isAlpha(v: any): boolean {
        return typeof v.name === "string" && typeof v.char === "string" && typeof v.bool === "boolean";
      }
      """
    }
  }

  "ArrayClass" in {
    println(TsGuardRenderer.renderPred(decl[ArrayClass]))
    TsGuardRenderer.renderPred(decl[ArrayClass]) shouldBe {
      i"""
      export function isArrayClass(v: any): boolean {
        return Array.isArray(v.aList) && v.aList.map((i) => (typeof i === "string")).reduce((a, b) => (a && b)) && (typeof v.optField === "number" || v.optField === null);
      }
      """
    }
  }

  "Numeric" in {
    TsGuardRenderer.renderPred(decl[Numeric]) shouldBe {
      i"""
      export function isNumeric(v: any): boolean {
        return typeof v.double === "number" && typeof v.float === "number" && typeof v.int === "number";
      }
      """
    }
  }

  "ClassOrObject" in {
    TsGuardRenderer.renderPred(decl[ClassOrObject]) shouldBe {
      i"""
      export function isClassOrObject(v: any): boolean {
        return v.type === "MyClass" ? typeof v.value === "number" : v.type === "MyObject" ? true : false;
      }
      """
    }
  }

  "NestedClassOrObject" in {
    TsGuardRenderer.renderPred(decl[NestedClassOrObject]) shouldBe {
      i"""
      export function isNestedClassOrObject(v: any): boolean {
        return v.type === "MyClass" ? typeof v.value === "number" : v.type === "MyObject" ? true : false;
      }
      """
    }
  }

  "Navigation" in {
    TsGuardRenderer.renderPred(decl[Navigation]) shouldBe {
      i"""
      export function isNavigation(v: any): boolean {
        return v.type === "Node" ? typeof v.name === "string" && Array.isArray(v.children) && v.children.map((i) => isNavigation(i)).reduce((a, b) => (a && b)) : v.type === "NodeList" ? Array.isArray(v.all) && v.all.map((i) => isNavigation(i)).reduce((a, b) => (a && b)) : false;
      }
      """
    }
  }

  "ClassUUID" in {
    TsGuardRenderer.renderPred(decl[ClassUUID]) shouldBe {
      i"""
      export function isClassUUID(v: any): boolean {
        return isUUID(v.a);
      }
      """
    }
  }

  "ClassDate" in {
    TsGuardRenderer.renderPred(decl[ClassDate]) shouldBe {
      i"""
      export function isClassDate(v: any): boolean {
        return isDate(v.a);
      }
      """
    }
  }

  "Recursive" in {
    TsGuardRenderer.renderPred(decl[Recursive]) shouldBe {
      i"""
      export function isRecursive(v: any): boolean {
        return typeof v.head === "number" && (isRecursive(v.tail) || v.tail === null);
      }
      """
    }
  }

  "Recursive2" in {
    TsGuardRenderer.renderPred(decl[Recursive2]) shouldBe {
      i"""
      export function isRecursive2(v: any): boolean {
        return typeof v.head === "number" && Array.isArray(v.tail) && v.tail.map((i) => isRecursive2(i)).reduce((a, b) => (a && b));
      }
      """
    }
  }

  "ExternalReferences" in {
    TsGuardRenderer.renderPred(decl[ExternalReferences]) shouldBe {
      i"""
      export function isExternalReferences(v: any): boolean {
        return isColor(v.color) && isNavigation(v.nav);
      }
      """
    }
  }

  "ObjectsOnly" in {
    TsGuardRenderer.renderPred(decl[ObjectsOnly]) shouldBe {
      i"""
      export function isObjectsOnly(v: any): boolean {
        return v.type === "ObjectOne" ? true : v.type === "ObjectTwo" ? true : false;
      }
      """
    }
  }

  "Union of Union" in {
    TsGuardRenderer.renderPred("A" := Ref("B") | Ref("C") | Ref("D")) shouldBe {
      i"""
      export function isA(v: any): boolean {
        return isB(v) || isC(v) || isD(v);
      }
      """
    }
  }

  "Inter of Inter" in {
    TsGuardRenderer.renderPred("A" := Ref("B") & Ref("C") & Ref("D")) shouldBe {
      i"""
      export function isA(v: any): boolean {
        return isB(v) && isC(v) && isD(v);
      }
      """
    }
  }
}
