package bridges

import bridges.SampleTypes._
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
           encoder : Color -> Encode.Value
           encoder obj = Encode.object [ ("red", Encode.int obj.red), ("green", Encode.int obj.green), ("blue", Encode.int obj.blue) ]
           """
      }

      "Circle" in {
        jsonEncoder[Elm](declaration[Circle]) shouldBe
          i"""
           encoder : Circle -> Encode.Value
           encoder obj = Encode.object [ ("radius", Encode.float obj.radius), ("color", Color.encoder obj.color) ]
           """
      }

      "Rectangle" in {
        jsonEncoder[Elm](declaration[Rectangle]) shouldBe
          i"""
           encoder : Rectangle -> Encode.Value
           encoder obj = Encode.object [ ("width", Encode.float obj.width), ("height", Encode.float obj.height), ("color", Color.encoder obj.color) ]
           """
      }

      "Shape" in {
        jsonEncoder[Elm](declaration[Shape]) shouldBe
          i"""
           encoder : Shape -> Encode.Value
           encoder tpe =
              case tpe of
                 Circle radius color -> Encode.object [ ("radius", Encode.float radius), ("color", Color.encoder color), ("type", Encode.string "Circle") ]
                 Rectangle width height color -> Encode.object [ ("width", Encode.float width), ("height", Encode.float height), ("color", Color.encoder color), ("type", Encode.string "Rectangle") ]
                 ShapeGroup leftShape rightShape -> Encode.object [ ("leftShape", encoder leftShape), ("rightShape", encoder rightShape), ("type", Encode.string "ShapeGroup") ]
           """
      }

      "Alpha" in {
        jsonEncoder[Elm](declaration[Alpha]) shouldBe
          i"""
           encoder : Alpha -> Encode.Value
           encoder obj = Encode.object [ ("name", Encode.string obj.name), ("char", Encode.string obj.char), ("bool", Encode.bool obj.bool) ]
           """
      }

      "ArrayClass" in {
        jsonEncoder[Elm](declaration[ArrayClass]) shouldBe
          i"""
           encoder : ArrayClass -> Encode.Value
           encoder obj = Encode.object [ ("aList", Encode.list (List.map Encode.string obj.aList)), ("optField", Maybe.withDefault Encode.null (Maybe.map Encode.float obj.optField)) ]
           """
      }

      "Numeric" in {
        jsonEncoder[Elm](declaration[Numeric]) shouldBe
          i"""
           encoder : Numeric -> Encode.Value
           encoder obj = Encode.object [ ("double", Encode.float obj.double), ("float", Encode.float obj.float), ("int", Encode.int obj.int) ]
           """
      }

      "ClassOrObject" in {
        jsonEncoder[Elm](declaration[ClassOrObject]) shouldBe
          i"""
           encoder : ClassOrObject -> Encode.Value
           encoder tpe =
              case tpe of
                 MyClass value -> Encode.object [ ("value", Encode.int value), ("type", Encode.string "MyClass") ]
                 MyObject -> Encode.object [ ("type", Encode.string "MyObject") ]
           """
      }

      "Navigation" in {
        jsonEncoder[Elm](declaration[Navigation]) shouldBe
          i"""
           encoder : Navigation -> Encode.Value
           encoder tpe =
              case tpe of
                 Node name children -> Encode.object [ ("name", Encode.string name), ("children", Encode.list (List.map encoder children)), ("type", Encode.string "Node") ]
                 NodeList all -> Encode.object [ ("all", Encode.list (List.map encoder all)), ("type", Encode.string "NodeList") ]
           """
      }

      "ClassUUID" in {
        jsonEncoder[Elm](declaration[ClassUUID]) shouldBe
          i"""
           encoder : ClassUUID -> Encode.Value
           encoder obj = Encode.object [ ("a", Uuid.encode obj.a) ]
           """
      }

      "ExternalReferences" in {
        jsonEncoder[Elm](declaration[ExternalReferences]) shouldBe
          i"""
           encoder : ExternalReferences -> Encode.Value
           encoder obj = Encode.object [ ("color", Color.encoder obj.color), ("nav", Navigation.encoder obj.nav) ]
           """
      }

      "MyUUID" in {
        // we want to treat UUID as string, using an override
        implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
          Encoder.pure(Str)

        jsonEncoder[Elm](declaration[MyUUID]) shouldBe
          i"""
           encoder : MyUUID -> Encode.Value
           encoder obj = Encode.object [ ("uuid", Encode.string obj.uuid) ]
           """
      }

    }
  }

}
