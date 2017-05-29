package bridges

import org.scalatest._
import unindent._

object RendererSpec {
  import syntax._

  final case class Color(red: Int, green: Int, blue: Int)

  sealed abstract class Shape extends Product with Serializable
  case class Rectangle(width: Double, height: Double, color: Color) extends Shape
  case class Circle(radius: Double, color: Color) extends Shape

  val customDeclaration =
    "Message" := Type.discUnion("level")(
      "error"   -> Type.Ref("ErrorMessage"),
      "warning" -> Type.Ref("WarningMessage")
    )
}

class RendererSpec extends FreeSpec with Matchers {
  import RendererSpec._
  import syntax._

  "render" - {
    "typescript" in {
      val actual: String =
        render[Typescript](List(
          declaration[Color],
          declaration[Circle],
          declaration[Rectangle],
          declaration[Shape],
          customDeclaration
        ))

      val expected: String =
        i"""
        export type Color = { red: number, green: number, blue: number };

        export type Circle = { radius: number, color: Color };

        export type Rectangle = { width: number, height: number, color: Color };

        export type Shape = (({ type: "Circle" } & Circle) | ({ type: "Rectangle" } & Rectangle));

        export type Message = (({ level: "error" } & ErrorMessage) | ({ level: "warning" } & WarningMessage));
        """

      actual should be(expected)
    }

    "flow" in {
      val actual: String =
        render[Flow](List(
          declaration[Color],
          declaration[Circle],
          declaration[Rectangle],
          declaration[Shape]
        ))

      val expected: String =
        i"""
        export type Color = { red: number, green: number, blue: number };

        export type Circle = { radius: number, color: Color };

        export type Rectangle = { width: number, height: number, color: Color };

        export type Shape = (({ type: "Circle" } & Circle) | ({ type: "Rectangle" } & Rectangle));
        """

      actual should be(expected)
    }
  }

}
