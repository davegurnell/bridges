package bridges.ts

import org.scalatest._

object TypeSpec {
  final case class Color(red: Int, green: Int, blue: Int)

  sealed abstract class Shape extends Product with Serializable
  case class Rectangle(width: Double, height: Double, color: Color) extends Shape
  case class Circle(radius: Double, color: Color) extends Shape
}

class TypeSpec extends FreeSpec with Matchers {
  import Type._
  import TypeSpec._

  "flatten" in {
    val actual   = encode[Shape].flatten
    val expected = Ref("Shape")

    actual should be(expected)
  }

  "bindings" in {
    val color = Binding("Color", Struct(
      "red"   -> Num,
      "green" -> Num,
      "blue"  -> Num
    ))

    val circle = Binding("Circle", Struct(
      "type"   -> StrLiteral("Circle"),
      "radius" -> Num,
      "color"  -> Ref("Color")
    ))

    val rectangle = Binding("Rectangle", Struct(
      "type"   -> StrLiteral("Rectangle"),
      "width"  -> Num,
      "height" -> Num,
      "color"  -> Ref("Color")
    ))

    val shape = Binding("Shape", Ref("Circle") | Ref("Rectangle"))

    val actual   = bindings[Shape]
    val expected = List(color, circle, rectangle, shape)

    actual should be(expected)
  }
}
