package bridges

import java.util.UUID

import bridges.Type.Struct
import syntax._

object SampleTypes {
  // Sample product
  case class Pair(a: String, b: Int)

  // Sample coproduct
  sealed abstract class OneOrOther extends Product with Serializable
  case class One(value: String) extends OneOrOther
  case class Other(value: Int) extends OneOrOther

  // Coproduct with Object
  sealed trait ClassOrObject extends Product with Serializable
  case class MyClass(value: Int) extends ClassOrObject
  case object MyObject extends ClassOrObject

  // Coproduct with Object in a nested object
  sealed abstract class NestedClassOrObject extends Product with Serializable
  object NestedClassOrObject {
    case class MyClass(value: Int) extends NestedClassOrObject
    case object MyObject extends NestedClassOrObject
  }

  // Sample value class
  case class Value(value: String) extends AnyVal

  // Sample UUID
  case class ClassUUID(a: UUID)

  // ADT with intermediate type appearing more than once:
  final case class Color(red: Int, green: Int, blue: Int)
  sealed abstract class Shape extends Product with Serializable
  final case class Circle(radius: Double, color: Color) extends Shape
  final case class Rectangle(width: Double, height: Double, color: Color)
      extends Shape
  final case class ShapeGroup(leftShape: Shape, rightShape: Shape) extends Shape

  // Recursive structure
  sealed trait Navigation
  final case class NodeList(all: List[Navigation]) extends Navigation
  final case class Node(name: String, children: List[Navigation])
      extends Navigation

  // case classes with specific values (list, Float, Option, Char, etc)
  final case class Alpha(name: String, char: Char, bool: Boolean)
  final case class ArrayClass(aList: List[String], optField: Option[Float])
  final case class Numeric(double: Double, float: Float, int: Int)
  final case class MyUUID(uuid: UUID)

  // case class whose members are other case classes not in its adt
  final case class ExternalReferences(color: Color, nav: Navigation)

  // mutually recursive types
  final case class TypeOne(name: String, values: List[TypeTwo])

  sealed trait TypeTwo
  final case class OptionOne(value: Int) extends TypeTwo
  final case class OptionTwo(value: TypeOne) extends TypeTwo

  sealed trait ObjectsOnly
  final case object ObjectOne extends ObjectsOnly
  final case object ObjectTwo extends ObjectsOnly

  // Custom declaration of a intermediate structure
  val customDeclaration: Declaration =
    "Message" := Type.discUnion("level")(
      ("error", Type.Ref("ErrorMessage"), Struct(Nil)),
      ("warning", Type.Ref("WarningMessage"), Struct(Nil))
    )
}
