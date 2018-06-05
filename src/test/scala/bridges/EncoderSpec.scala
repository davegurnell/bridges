package bridges

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
  final case class ShapeGroup(leftShape: Shape, rightShape: Shape) extends Shape

  // Recursive structure
  sealed trait Navigation
  final case class NodeList(all: List[Navigation]) extends Navigation
  final case class Node(name: String, children: List[Navigation]) extends Navigation
}

class EncoderSpec extends FreeSpec with Matchers {
  import syntax._
  import EncoderSpec._
  import Type._

  "encode[A]" - {
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

    "sequences" in {
      encode[Seq[String]] should be(Array(Str))
      encode[Set[Set[Int]]] should be(Array(Array(Num)))
    }

    "value classes" in {
      encode[Value] should be(Str)
    }

    "case classes" in {
      encode[Pair] should be(Struct("a" -> Str, "b" -> Num))
    }

    "sealed types" in {
      encode[OneOrOther] should be(discUnion("One" -> Ref("One"), "Other" -> Ref("Other")))
    }

    "overridden defaults" in {
      implicit val oneEncoder: BasicEncoder[One] =
        Encoder.pure(Str)

      encode[One] should be(Str)
      encode[OneOrOther] should be(discUnion("One" -> Str, "Other" -> Ref("Other")))
    }

    "sealed types with intermediate types and indirect recursion" in {
      encode[Shape] should be(discUnion("Circle" -> Ref("Circle"), "Rectangle" -> Ref("Rectangle"), "ShapeGroup" -> Ref("ShapeGroup")))
      encode[Circle] should be(Struct("radius" -> Num, "color" -> Ref("Color")))
      encode[Rectangle] should be(Struct("width" -> Num, "height" -> Num, "color" -> Ref("Color")))
      encode[ShapeGroup] should be(Struct("leftShape" ->  Ref("Shape"), "rightShape" -> Ref("Shape")))
    }

    "recursive types with direct recursion on same type" in {
      encode[Navigation] should be(discUnion("Node" -> Ref("Node"), "NodeList" -> Ref("NodeList")))
      encode[NodeList] should be(Struct("all" -> Array(Ref("Navigation"))))
      encode[Node] should be(Struct("name" -> Str, "children" -> Array(Ref("Navigation"))))
    }
  }

  "declaration[A]" - {
    "value classes" in {
      declaration[Value] should be("Value" := Str)
    }

    "case classes" in {
      declaration[Pair] should be("Pair" := Struct("a" -> Str, "b" -> Num))
    }

    "sealed types" in {
      declaration[OneOrOther] should be("OneOrOther" := discUnion("One" -> Ref("One"), "Other" -> Ref("Other")))
    }

    "overridden defaults" in {
      implicit val oneEncoder: BasicEncoder[One] =
        Encoder.pure(Str)

      encode[One] should be(Str)
      declaration[OneOrOther] should be("OneOrOther" := discUnion("One" -> Str, "Other" -> Ref("Other")))
    }
  }
}
