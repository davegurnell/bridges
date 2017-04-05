package bridges.ts

import org.scalatest._
import unindent._

object RendererSpec {
  final case class Color(red: Int, green: Int, blue: Int)

  sealed abstract class Shape extends Product with Serializable
  case class Rectangle(width: Double, height: Double, color: Color) extends Shape
  case class Circle(radius: Double, color: Color) extends Shape
}

class RendererSpec extends FreeSpec with Matchers {
  import RendererSpec._

  "render" in {
    val actual: String =
      bindings[Shape].render

    val expected: String =
      i"""
      type Color = {
        red: number,
        green: number,
        blue: number
      }

      type Circle = {
        radius: number,
        color: Color
      }

      type Rectangle = {
        width: number,
        height: number,
        color: Color
      }

      type Shape =
        Circle |
        Rectangle
      """

    actual should be(expected)
  }

}
