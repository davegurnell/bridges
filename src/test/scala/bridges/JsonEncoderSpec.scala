package bridges

import types.SampleTypes._
import bridges.Type.Str
import syntax._
import org.scalatest._
import unindent._

class JsonEncoderSpec extends FreeSpec with Matchers {

  "encoder" - {
    "elm" - {

      "Color" in {
        jsonEncoder[Elm](declaration[Color]) shouldBe
          i"""
           encoderColor : Color -> Encode.Value
           encoderColor obj = Encode.object [ ("red", Encode.int obj.red), ("green", Encode.int obj.green), ("blue", Encode.int obj.blue) ]
           """
      }

      "Circle" in {
        jsonEncoder[Elm](declaration[Circle]) shouldBe
          i"""
           encoderCircle : Circle -> Encode.Value
           encoderCircle obj = Encode.object [ ("radius", Encode.float obj.radius), ("color", encoderColor obj.color) ]
           """
      }

      "Rectangle" in {
        jsonEncoder[Elm](declaration[Rectangle]) shouldBe
          i"""
           encoderRectangle : Rectangle -> Encode.Value
           encoderRectangle obj = Encode.object [ ("width", Encode.float obj.width), ("height", Encode.float obj.height), ("color", encoderColor obj.color) ]
           """
      }

      "Shape" in {
        jsonEncoder[Elm](declaration[Shape]) shouldBe
          i"""
           encoderShape : Shape -> Encode.Value
           encoderShape tpe =
              case tpe of
                 Circle radius color -> Encode.object [ ("radius", Encode.float radius), ("color", encoderColor color), ("type", Encode.string "Circle") ]
                 Rectangle width height color -> Encode.object [ ("width", Encode.float width), ("height", Encode.float height), ("color", encoderColor color), ("type", Encode.string "Rectangle") ]
                 ShapeGroup leftShape rightShape -> Encode.object [ ("leftShape", encoderShape leftShape), ("rightShape", encoderShape rightShape), ("type", Encode.string "ShapeGroup") ]
           """
      }

      "Alpha" in {
        jsonEncoder[Elm](declaration[Alpha]) shouldBe
          i"""
           encoderAlpha : Alpha -> Encode.Value
           encoderAlpha obj = Encode.object [ ("name", Encode.string obj.name), ("char", Encode.string obj.char), ("bool", Encode.bool obj.bool) ]
           """
      }

      "ArrayClass" in {
        jsonEncoder[Elm](declaration[ArrayClass]) shouldBe
          i"""
           encoderArrayClass : ArrayClass -> Encode.Value
           encoderArrayClass obj = Encode.object [ ("aList", Encode.list (List.map Encode.string obj.aList)), ("optField", Maybe.withDefault Encode.null (Maybe.map Encode.float obj.optField)) ]
           """
      }

      "Numeric" in {
        jsonEncoder[Elm](declaration[Numeric]) shouldBe
          i"""
           encoderNumeric : Numeric -> Encode.Value
           encoderNumeric obj = Encode.object [ ("double", Encode.float obj.double), ("float", Encode.float obj.float), ("int", Encode.int obj.int) ]
           """
      }

      "ClassOrObject" in {
        jsonEncoder[Elm](declaration[ClassOrObject]) shouldBe
          i"""
           encoderClassOrObject : ClassOrObject -> Encode.Value
           encoderClassOrObject tpe =
              case tpe of
                 MyClass value -> Encode.object [ ("value", Encode.int value), ("type", Encode.string "MyClass") ]
                 MyObject -> Encode.object [ ("type", Encode.string "MyObject") ]
           """
      }

      "Navigation" in {
        jsonEncoder[Elm](declaration[Navigation]) shouldBe
          i"""
           encoderNavigation : Navigation -> Encode.Value
           encoderNavigation tpe =
              case tpe of
                 Node name children -> Encode.object [ ("name", Encode.string name), ("children", Encode.list (List.map encoderNavigation children)), ("type", Encode.string "Node") ]
                 NodeList all -> Encode.object [ ("all", Encode.list (List.map encoderNavigation all)), ("type", Encode.string "NodeList") ]
           """
      }

      "ClassUUID" in {
        jsonEncoder[Elm](declaration[ClassUUID]) shouldBe
          i"""
           encoderClassUUID : ClassUUID -> Encode.Value
           encoderClassUUID obj = Encode.object [ ("a", Uuid.encode obj.a) ]
           """
      }

      "ExternalReferences" in {
        jsonEncoder[Elm](declaration[ExternalReferences]) shouldBe
          i"""
           encoderExternalReferences : ExternalReferences -> Encode.Value
           encoderExternalReferences obj = Encode.object [ ("color", encoderColor obj.color), ("nav", encoderNavigation obj.nav) ]
           """
      }

      "TypeOne and TypeTwo" in {
        jsonEncoder[Elm](List(declaration[TypeOne], declaration[TypeTwo])) shouldBe
          i"""
           encoderTypeOne : TypeOne -> Encode.Value
           encoderTypeOne obj = Encode.object [ ("name", Encode.string obj.name), ("values", Encode.list (List.map encoderTypeTwo obj.values)) ]

           encoderTypeTwo : TypeTwo -> Encode.Value
           encoderTypeTwo tpe =
              case tpe of
                 OptionOne value -> Encode.object [ ("value", Encode.int value), ("type", Encode.string "OptionOne") ]
                 OptionTwo value -> Encode.object [ ("value", encoderTypeOne value), ("type", Encode.string "OptionTwo") ]
           """
      }

      "MyUUID" in {
        // we want to treat UUID as string, using an override
        implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
          Encoder.pure(Str)

        jsonEncoder[Elm](declaration[MyUUID]) shouldBe
          i"""
           encoderMyUUID : MyUUID -> Encode.Value
           encoderMyUUID obj = Encode.object [ ("uuid", Encode.string obj.uuid) ]
           """
      }

      "ObjectsOnly" in {
        jsonEncoder[Elm](List(declaration[ObjectsOnly])) shouldBe
          i"""
           encoderObjectsOnly : ObjectsOnly -> Encode.Value
           encoderObjectsOnly tpe =
              case tpe of
                 ObjectOne -> Encode.object [ ("type", Encode.string "ObjectOne") ]
                 ObjectTwo -> Encode.object [ ("type", Encode.string "ObjectTwo") ]
           """
      }

    }
  }

}
