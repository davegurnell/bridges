package bridges

import bridges.Type.Struct
import org.scalatest._
import unindent._

object RendererSpec {
  import syntax._

  final case class Color(red: Int, green: Int, blue: Int)

  sealed abstract class Shape extends Product with Serializable
  case class Rectangle(width: Double, height: Double, color: Color)
      extends Shape
  case class Circle(radius: Double, color: Color) extends Shape

  final case class Alpha(name: String, char: Char, bool: Boolean)
  final case class ArrayClass(aList: List[String], optField: Option[Float])
  final case class Numeric(double: Double, float: Float, int: Int)

  sealed abstract class ClassOrObject extends Product with Serializable
  case class MyClass(value: Int) extends ClassOrObject
  case object MyObject extends ClassOrObject

  val customDeclaration: Declaration =
    "Message" := Type.discUnion("level")(
      ("error", Type.Ref("ErrorMessage"), Struct(Nil)),
      ("warning", Type.Ref("WarningMessage"), Struct(Nil))
    )
}

class RendererSpec extends FreeSpec with Matchers {
  import RendererSpec._
  import syntax._

  "render" - {
    "typescript" in {
      val actual: String =
        render[Typescript](
          List(
            declaration[Color],
            declaration[Circle],
            declaration[Rectangle],
            declaration[Shape],
            declaration[Alpha],
            declaration[ArrayClass],
            declaration[Numeric],
            declaration[ClassOrObject],
            customDeclaration
          )
        )

      val expected: String =
        i"""
        export type Color = { red: number, green: number, blue: number };

        export type Circle = { radius: number, color: Color };

        export type Rectangle = { width: number, height: number, color: Color };

        export type Shape = (({ type: "Circle" } & Circle) | ({ type: "Rectangle" } & Rectangle));

        export type Alpha = { name: string, char: string, bool: boolean };

        export type ArrayClass = { aList: Array<string>, optField: (number | null) };

        export type Numeric = { double: number, float: number, int: number };

        export type ClassOrObject = (({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject));

        export type Message = (({ level: "error" } & ErrorMessage) | ({ level: "warning" } & WarningMessage));
        """

      actual should be(expected)
    }

    "flow" in {
      val actual: String =
        render[Flow](
          List(
            declaration[Color],
            declaration[Circle],
            declaration[Rectangle],
            declaration[Shape],
            declaration[Alpha],
            declaration[ArrayClass],
            declaration[Numeric],
            declaration[ClassOrObject]
          )
        )

      val expected: String =
        i"""
        export type Color = { red: number, green: number, blue: number };

        export type Circle = { radius: number, color: Color };

        export type Rectangle = { width: number, height: number, color: Color };

        export type Shape = (({ type: "Circle" } & Circle) | ({ type: "Rectangle" } & Rectangle));

        export type Alpha = { name: string, char: string, bool: boolean };

        export type ArrayClass = { aList: Array<string>, optField: (number | null) };

        export type Numeric = { double: number, float: number, int: number };

        export type ClassOrObject = (({ type: "MyClass" } & MyClass) | ({ type: "MyObject" } & MyObject));
        """

      actual should be(expected)
    }

    "elm" in {
      val actual: String =
        render[Elm](
          List(
            declaration[Color],
            declaration[Circle],
            declaration[Rectangle],
            declaration[Shape],
            declaration[Alpha],
            declaration[ArrayClass],
            declaration[Numeric],
            declaration[ClassOrObject]
          )
        )

      val expected: String =
        i"""
        type alias Color = { red: Int, green: Int, blue: Int }

        type alias Circle = { radius: Float, color: Color }

        type alias Rectangle = { width: Float, height: Float, color: Color }

        type Shape = Circle Float Color | Rectangle Float Float Color

        type alias Alpha = { name: String, char: Char, bool: Bool }

        type alias ArrayClass = { aList: List String, optField: Maybe Float }

        type alias Numeric = { double: Float, float: Float, int: Int }

        type ClassOrObject = MyClass Int | MyObject
        """

      actual should be(expected)
    }
  }

}
