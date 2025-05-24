package bridges

import bridges.TsType.*
import bridges.syntax.*
import bridges.SampleTypes.*
import munit.FunSuite

class TsEncoderSuite extends FunSuite {
  test("primitive types") {
    assertEquals(encode[String], TsType.Str)
    assertEquals(encode[Char], TsType.Str)
    assertEquals(encode[Int], TsType.Num)
    assertEquals(encode[Float], TsType.Num)
    assertEquals(encode[Double], TsType.Num)
    assertEquals(encode[Boolean], TsType.Bool)
  }

  test("options") {
    assertEquals(encode[Option[String]], TsType.Str | TsType.Null)
    assertEquals(encode[Option[Int]], TsType.Num | TsType.Null)
  }

  test("sequences") {
    assertEquals(encode[Seq[String]], TsType.Arr(TsType.Str))
    assertEquals(encode[Set[Set[Int]]], TsType.Arr(TsType.Arr(TsType.Num)))
  }

  test("value classes".ignore) {
    assertEquals(encode[Value], TsType.Str)
  }

  test("a class with UUID member") {
    assertEquals(
      encode[ClassUUID],
      struct(
        "a" ---> TsType.Ref("UUID")
      )
    )
  }

  test("a class with Date member") {
    assertEquals(
      encode[ClassDate],
      struct(
        "a" ---> TsType.Ref("Date")
      )
    )
  }

  test("case classes") {
    assertEquals(
      encode[Pair],
      struct(
        "a" ---> TsType.Str,
        "b" ---> TsType.Num
      )
    )
  }

  test("sealed types") {
    assertEquals(
      encode[OneOrOther],
      union(
        TsType.discriminated("One"),
        TsType.discriminated("Other"),
      )
    )
  }

  test("sealed types with objects") {
    assertEquals(
      encode[ClassOrObject],
      union(
        TsType.discriminated("MyClass"),
        TsType.discriminated("MyObject"),
      )
    )
  }

  test("sealed types with objects in nested objects") {
    assertEquals(
      encode[NestedClassOrObject],
      union(
        TsType.discriminated("MyClass"),
        TsType.discriminated("MyObject"),
      )
    )
  }

  test("overridden defaults") {
    implicit val oneEncoder: TsEncoder[One] =
      TsEncoder.instance(Str)

    assertEquals(encode[One], Str)

    assertEquals(
      encode[OneOrOther],
      union(
        TsType.discriminated("One", Str),
        TsType.discriminated("Other"),
      )
    )
  }

  test("sealed types with intermediate types and indirect recursion") {
    assertEquals(
      encode[Shape],
      union(
        TsType.discriminated("Circle"),
        TsType.discriminated("Rectangle"),
        TsType.discriminated("ShapeGroup"),
      )
    )

    assertEquals(
      encode[Circle],
      struct(
        "radius" ---> Num,
        "color"  ---> Ref("Color")
      )
    )

    assertEquals(
      encode[Rectangle],
      struct(
        "width"  ---> Num,
        "height" ---> Num,
        "color"  ---> Ref("Color")
      )
    )

    assertEquals(
      encode[ShapeGroup],
      struct(
        "leftShape"  ---> Ref("Shape"),
        "rightShape" ---> Ref("Shape")
      )
    )
  }

  test("recursive types with direct recursion on same type") {
    assertEquals(
      encode[Navigation],
      union(
        TsType.discriminated("NodeList"),
        TsType.discriminated("Node"),
      )
    )

    assertEquals(
      encode[NodeList],
      struct(
        "all" ---> array(Ref("Navigation"))
      )
    )

    assertEquals(
      encode[Node],
      struct(
        "name"     ---> Str,
        "children" ---> array(Ref("Navigation"))
      )
    )
  }

  test("types with specific parameters") {
    assertEquals(
      encode[Alpha],
      struct(
        "name" ---> Str,
        "char" ---> Str,
        "bool" ---> Bool
      )
    )

    assertEquals(
      encode[ArrayClass],
      struct(
        "aList" ---> array(Str),
        "optField" --?> nullable(Num)
      )
    )
    assertEquals(
      encode[Numeric],
      struct(
        "double" ---> Num,
        "float"  ---> Num,
        "int"    ---> Num
      )
    )
  }

  test("class that references other case classes") {
    assertEquals(
      encode[ExternalReferences],
      struct(
        "color" ---> Ref("Color"),
        "nav"   ---> Ref("Navigation")
      )
    )
  }

  test("mutually recursive types") {
    assertEquals(
      encode[TypeOne],
      struct(
        "name"   ---> Str,
        "values" ---> array(Ref("TypeTwo"))
      )
    )

    assertEquals(
      encode[TypeTwo],
      union(
        TsType.discriminated("OptionOne"),
        TsType.discriminated("OptionTwo"),
      )
    )
  }

  test("self-recursive type") {
    assertEquals(
      encode[Recursive],
      struct(
        "head" ---> Num,
        "tail" -??> ref("Recursive")
      )
    )

    assertEquals(
      encode[Recursive2],
      struct(
        "head" ---> Num,
        "tail" ---> array(Ref("Recursive2"))
      )
    )
  }

  test("pure objects ADT") {
    assertEquals(
      encode[ObjectsOnly],
      union(
        TsType.discriminated("ObjectOne"),
        TsType.discriminated("ObjectTwo")
      )
    )
  }

  test("we can override uuid as string") {
    @scala.annotation.nowarn("msg=unused local definition")
    implicit val uuidEncoder: TsEncoder[java.util.UUID] =
      TsEncoder.instance(Str)

    assertEquals(encode[ClassUUID], struct("a" ---> Str))
  }

  test("declarations - value classes".ignore) {
    assertEquals(decl[Value], decl("Value")(Str))
  }

  test("declarations - case classes") {
    assertEquals(
      decl[Pair],
      decl("Pair")(struct(
        "a" ---> Str,
        "b" ---> Num
      ))
    )
  }

  test("declarations - sealed types") {
    assertEquals(
      decl[OneOrOther],
      decl("OneOrOther")(union(
        TsType.discriminated("One"),
        TsType.discriminated("Other")
      ))
    )
  }

  test("declarations - overridden defaults") {
    implicit val oneEncoder: TsEncoder[One] =
      TsEncoder.instance(Str)

    assertEquals(encode[One], Str)

    assertEquals(
      decl[OneOrOther],
      decl("OneOrOther")(union(
        TsType.discriminated("One", Str),
        TsType.discriminated("Other")
      ))
    )
  }

  test("Numeric types") {
    assertEquals(
      decl[NumericTypes],
      decl("NumericTypes")(
        struct(
          "int"        ---> Num,
          "long"       ---> Num,
          "float"      ---> Num,
          "double"     ---> Num,
          "bigDecimal" ---> Num
        )
      )
    )
  }

  test("Map") {
    assertEquals(decl[Map[String, Int]], decl("Map")(record(Str, Num)))
    assertEquals(
      decl[Map[String, Pair]],
      decl("Map")(record(Str, Ref("Pair")))
    )
  }

  test("class with field with custom instance") {
    given renameColorToColour: RefEncoder[Color] =
      RefEncoder.instance(ref("Colour"))

    assertEquals(
      encode[Circle],
      struct(
        "radius" ---> Num,
        "color"  ---> ref("Colour")
      )
    )
  }
}
