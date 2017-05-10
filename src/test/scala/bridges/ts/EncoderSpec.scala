package bridges.ts

import org.scalatest._

object EncoderSpec {
  // Sample product
  case class Pair(a: String, b: Int)

  // Sample coproduct
  sealed abstract class OneOrOther extends Product with Serializable
  case class One(value: String) extends OneOrOther
  case class Other(value: Int) extends OneOrOther

  // Sample value class
  case class Value(value: String) extends AnyVal

  // ADT with intermediate type appearing more than once:
  final case class Color(red: Int, green: Int, blue: Int)
  sealed abstract class Shape extends Product with Serializable
  final case class Circle(radius: Double, color: Color) extends Shape
  final case class Rectangle(width: Double, height: Double, color: Color) extends Shape
}

class EncoderSpec extends FreeSpec with Matchers {
  import Type._
  import EncoderSpec._

  "primitive types" in {
    encode[String] should be(Str)
    encode[Int] should be(Num)
    encode[Double] should be(Num)
    encode[Boolean] should be(Bool)
  }

  "options" in {
    encode[Option[String]] should be(Str | Null)
    encode[Option[Int]] should be(Num | Null)
  }

  "case classes" in {
    val actual   = encode[Pair]
    val expected = Binding("Pair", Struct("a" -> Str, "b" -> Num))
    actual should be(expected)
  }

  "sealed types" in {
    val actual   = encode[OneOrOther]
    val one      = encode[One]
    val other    = encode[Other]
    val expected = Binding("OneOrOther", (
      Binding("One",   Struct("type" -> StrLiteral("One"),   "value" -> Str)) |
      Binding("Other", Struct("type" -> StrLiteral("Other"), "value" -> Num))
    ))
    actual should be(expected)
  }

  // TODO: The value class encoder is causing implicit divergence:
  // "value classes" in {
  //   val actual   = encode[Value]
  //   val expected = Binding("Value", Str)
  //   actual should be(expected)
  // }

  "multi-tier adt" in {
    val color = Binding("Color", Struct(
      "red"   -> Num,
      "green" -> Num,
      "blue"  -> Num
    ))

    val circle = Binding("Circle", Struct(
      "type"   -> StrLiteral("Circle"),
      "radius" -> Num,
      "color"  -> color
    ))

    val rectangle = Binding("Rectangle", Struct(
      "type"   -> StrLiteral("Rectangle"),
      "width"  -> Num,
      "height" -> Num,
      "color"  -> color
    ))

    val actual   = encode[Shape]
    val expected = Binding("Shape", circle | rectangle)

    actual should be(expected)
  }
}
