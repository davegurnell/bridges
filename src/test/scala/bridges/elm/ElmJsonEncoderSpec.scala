package bridges.elm

import bridges.SampleTypes._
import bridges.core._
import bridges.core.Type._
import bridges.core.syntax._
import org.scalatest._
import unindent._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ElmJsonEncoderSpec extends AnyFreeSpec with Matchers {
  "Color" in {
    Elm.encoder(decl[Color]) shouldBe {
      i"""
      encoderColor : Color -> Encode.Value
      encoderColor obj = Encode.object [ ("red", Encode.int obj.red), ("green", Encode.int obj.green), ("blue", Encode.int obj.blue) ]
      """
    }
  }

  "Circle" in {
    Elm.encoder(decl[Circle]) shouldBe {
      i"""
      encoderCircle : Circle -> Encode.Value
      encoderCircle obj = Encode.object [ ("radius", Encode.float obj.radius), ("color", encoderColor obj.color) ]
      """
    }
  }

  "Rectangle" in {
    Elm.encoder(decl[Rectangle]) shouldBe {
      i"""
      encoderRectangle : Rectangle -> Encode.Value
      encoderRectangle obj = Encode.object [ ("width", Encode.float obj.width), ("height", Encode.float obj.height), ("color", encoderColor obj.color) ]
      """
    }
  }

  "Shape" in {
    Elm.encoder(decl[Shape]) shouldBe {
      i"""
      encoderShape : Shape -> Encode.Value
      encoderShape tpe =
         case tpe of
            Circle radius color -> Encode.object [ ("radius", Encode.float radius), ("color", encoderColor color), ("type", Encode.string "Circle") ]
            Rectangle width height color -> Encode.object [ ("width", Encode.float width), ("height", Encode.float height), ("color", encoderColor color), ("type", Encode.string "Rectangle") ]
            ShapeGroup leftShape rightShape -> Encode.object [ ("leftShape", encoderShape leftShape), ("rightShape", encoderShape rightShape), ("type", Encode.string "ShapeGroup") ]
      """
    }
  }

  "Alpha" in {
    Elm.encoder(decl[Alpha]) shouldBe {
      i"""
      encoderAlpha : Alpha -> Encode.Value
      encoderAlpha obj = Encode.object [ ("name", Encode.string obj.name), ("char", Encode.string obj.char), ("bool", Encode.bool obj.bool) ]
      """
    }
  }

  "ArrayClass" in {
    Elm.encoder(decl[ArrayClass]) shouldBe {
      i"""
      encoderArrayClass : ArrayClass -> Encode.Value
      encoderArrayClass obj = Encode.object [ ("aList", Encode.list Encode.string obj.aList), ("optField", Maybe.withDefault Encode.null (Maybe.map Encode.float obj.optField)) ]
      """
    }
  }

  "Numeric" in {
    Elm.encoder(decl[Numeric]) shouldBe {
      i"""
      encoderNumeric : Numeric -> Encode.Value
      encoderNumeric obj = Encode.object [ ("double", Encode.float obj.double), ("float", Encode.float obj.float), ("int", Encode.int obj.int) ]
      """
    }
  }

  "ClassOrObject" in {
    Elm.encoder(decl[ClassOrObject]) shouldBe
    i"""
      encoderClassOrObject : ClassOrObject -> Encode.Value
      encoderClassOrObject tpe =
         case tpe of
            MyClass value -> Encode.object [ ("value", Encode.int value), ("type", Encode.string "MyClass") ]
            MyObject -> Encode.object [ ("type", Encode.string "MyObject") ]
      """
  }

  "Navigation" in {
    Elm.encoder(decl[Navigation]) shouldBe {
      i"""
      encoderNavigation : Navigation -> Encode.Value
      encoderNavigation tpe =
         case tpe of
            Node name children -> Encode.object [ ("name", Encode.string name), ("children", Encode.list encoderNavigation children), ("type", Encode.string "Node") ]
            NodeList all -> Encode.object [ ("all", Encode.list encoderNavigation all), ("type", Encode.string "NodeList") ]
      """
    }
  }

  "ExternalReferences" in {
    Elm.encoder(decl[ExternalReferences]) shouldBe {
      i"""
      encoderExternalReferences : ExternalReferences -> Encode.Value
      encoderExternalReferences obj = Encode.object [ ("color", encoderColor obj.color), ("nav", encoderNavigation obj.nav) ]
      """
    }
  }

  "TypeOne and TypeTwo" in {
    Elm.encoder(List(decl[TypeOne], decl[TypeTwo]), Map.empty[Ref, TypeReplacement]) shouldBe {
      i"""
      encoderTypeOne : TypeOne -> Encode.Value
      encoderTypeOne obj = Encode.object [ ("name", Encode.string obj.name), ("values", Encode.list encoderTypeTwo obj.values) ]

      encoderTypeTwo : TypeTwo -> Encode.Value
      encoderTypeTwo tpe =
         case tpe of
            OptionOne value -> Encode.object [ ("value", Encode.int value), ("type", Encode.string "OptionOne") ]
            OptionTwo value -> Encode.object [ ("value", encoderTypeOne value), ("type", Encode.string "OptionTwo") ]
      """
    }
  }

  "ClassUUID" - {
    "by default we treat UUID as a normal type we created" in {
      // this is the case when we don't import any Elm specific UUID library and we will create our own UUID type there

      Elm.encoder(decl[ClassUUID]) shouldBe {
        i"""
        encoderClassUUID : ClassUUID -> Encode.Value
        encoderClassUUID obj = Encode.object [ ("a", encoderUUID obj.a) ]
        """
      }
    }

    "we can provide a map to substitute UUID decoding with a custom encoding logic" in {
      // this is the case when we import Elm specific UUID types we want to use in our decoder, but Scala can't know about them without extra hints
      val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
        Ref("UUID") -> TypeReplacement("Uuid", "import Uuid exposing (Uuid)", "Uuid.decoder", "Uuid.encode")
      )

      Elm.encoder(decl[ClassUUID], customTypeReplacements) shouldBe {
        i"""
        encoderClassUUID : ClassUUID -> Encode.Value
        encoderClassUUID obj = Encode.object [ ("a", Uuid.encode obj.a) ]
        """
      }
    }

    "we can override the Encoder so we treat UUID as another basic type, like String, and encode it accordingly" in {
      // probably not recommended, better to use a mapping as in other tests, but it is supported
      implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
        Encoder.pure(Str)

      Elm.encoder(decl[ClassUUID]) shouldBe {
        i"""
        encoderClassUUID : ClassUUID -> Encode.Value
        encoderClassUUID obj = Encode.object [ ("a", Encode.string obj.a) ]
        """
      }
    }
  }

  "ClassDate" - {
    "by default we treat Date as a normal type we created" in {
      // this is the case when we don't import any Elm specific Date library and we will create our own Date type there

      Elm.encoder(decl[ClassDate]) shouldBe {
        i"""
        encoderClassDate : ClassDate -> Encode.Value
        encoderClassDate obj = Encode.object [ ("a", encoderDate obj.a) ]
        """
      }
    }

    "we can provide a map to substitute Date decoding with a custom encoding logic" in {
      // this is the case when we import Elm specific Date types we want to use in our decoder, but Scala can't know about them without extra hints
      val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
        Ref("Date") -> TypeReplacement("Date", "import Date exposing (Date)", "Date.decoder", "Date.encode")
      )

      Elm.encoder(decl[ClassDate], customTypeReplacements) shouldBe {
        i"""
        encoderClassDate : ClassDate -> Encode.Value
        encoderClassDate obj = Encode.object [ ("a", Date.encode obj.a) ]
        """
      }
    }

    "we can override the Encoder so we treat Date as another basic type, like String, and encode it accordingly" in {
      // probably not recommended, better to use a mapping as in other tests, but it is supported
      implicit val dateEncoder: BasicEncoder[java.util.Date] =
        Encoder.pure(Str)

      Elm.encoder(decl[ClassDate]) shouldBe {
        i"""
        encoderClassDate : ClassDate -> Encode.Value
        encoderClassDate obj = Encode.object [ ("a", Encode.string obj.a) ]
        """
      }
    }
  }

  "Recursive" in {
    Elm.encoder(decl[Recursive]) shouldBe {
      i"""
      encoderRecursive : Recursive -> Encode.Value
      encoderRecursive obj = Encode.object [ ("head", Encode.int obj.head), ("tail", Maybe.withDefault Encode.null (Maybe.map encoderRecursive obj.tail)) ]
      """
    }
  }

  "Recursive2" in {
    Elm.encoder(decl[Recursive2]) shouldBe {
      i"""
      encoderRecursive2 : Recursive2 -> Encode.Value
      encoderRecursive2 obj = Encode.object [ ("head", Encode.int obj.head), ("tail", Encode.list encoderRecursive2 obj.tail) ]
      """
    }
  }

  "ObjectsOnly" in {
    Elm.encoder(decl[ObjectsOnly]) shouldBe {
      i"""
      encoderObjectsOnly : ObjectsOnly -> Encode.Value
      encoderObjectsOnly tpe =
         case tpe of
            ObjectOne -> Encode.object [ ("type", Encode.string "ObjectOne") ]
            ObjectTwo -> Encode.object [ ("type", Encode.string "ObjectTwo") ]
      """
    }
  }

  "ClassWithGeneric" in {
    val productDef  = prod("first" -> Ref("A"), "second" -> Ref("B"), "third" -> Ref("C"))
    val declaration = decl("ClassWithGeneric", "A", "B", "C")(productDef)
    Elm.encoder(declaration) shouldBe {
      i"""
      encoderClassWithGeneric : (a -> Encode.Value) -> (b -> Encode.Value) -> (c -> Encode.Value) -> ClassWithGeneric a b c -> Encode.Value
      encoderClassWithGeneric encoderA encoderB encoderC obj = Encode.object [ ("first", encoderA obj.first), ("second", encoderB obj.second), ("third", encoderC obj.third) ]
      """
    }
  }

  "ClassWithGeneric2" in {
    val productDef  = prod("first" -> Ref("A"))
    val declaration = decl("ClassWithGeneric2", "A")(productDef)
    Elm.encoder(declaration) shouldBe {
      i"""
      encoderClassWithGeneric2 : (a -> Encode.Value) -> ClassWithGeneric2 a -> Encode.Value
      encoderClassWithGeneric2 encoderA obj = Encode.object [ ("first", encoderA obj.first) ]
      """
    }
  }

  "SumWithGeneric" in {
    val sumDef = sum(
      "First"  -> prod("f" -> Ref("A")),
      "Second" -> prod("s" -> Ref("B")),
      "Third"  -> prod("t" -> Ref("C"))
    )
    val declaration = decl("SumWithGeneric", "A", "B", "C")(sumDef)
    Elm.encoder(declaration) shouldBe {
      i"""
      encoderSumWithGeneric : (a -> Encode.Value) -> (b -> Encode.Value) -> (c -> Encode.Value) -> SumWithGeneric a b c -> Encode.Value
      encoderSumWithGeneric encoderA encoderB encoderC tpe =
         case tpe of
            First f -> Encode.object [ ("f", encoderA f), ("type", Encode.string "First") ]
            Second s -> Encode.object [ ("s", encoderB s), ("type", Encode.string "Second") ]
            Third t -> Encode.object [ ("t", encoderC t), ("type", Encode.string "Third") ]
      """
    }
  }

  "Numeric types" in {
    Elm.encoder(decl[NumericTypes]) shouldBe {
      i"""
      encoderNumericTypes : NumericTypes -> Encode.Value
      encoderNumericTypes obj = Encode.object [ ("int", Encode.int obj.int), ("long", Encode.int obj.long), ("float", Encode.float obj.float), ("double", Encode.float obj.double), ("bigDecimal", Encode.float obj.bigDecimal) ]
      """
    }
  }

  "Dictionary types" in {
    Elm.encoder(decl[Map[String, Int]]) shouldBe {
      i"""
      encoderMap : Map -> Encode.Value
      encoderMap obj = (Encode.dict Encode.string Map Encode.int Map)
      """
    }
  }
}
