package bridges

import bridges.SampleTypes._
import bridges.Type.Str
import bridges.syntax._
import org.scalatest._
import unindent._

class JsonDecoderSpec extends FreeSpec with Matchers {
  "decoder" - {
    "elm" - {

      "Color" in {
        jsonDecoder[Elm](declaration[Color]) shouldBe
          i"""
           decoderColor : Decode.Decoder Color
           decoderColor = decode Color |> required "red" Decode.int |> required "green" Decode.int |> required "blue" Decode.int
           """
      }

      "Circle" in {
        jsonDecoder[Elm](declaration[Circle]) shouldBe
          i"""
           decoderCircle : Decode.Decoder Circle
           decoderCircle = decode Circle |> required "radius" Decode.float |> required "color" decoderColor
           """
      }

      "Rectangle" in {
        jsonDecoder[Elm](declaration[Rectangle]) shouldBe
          i"""
           decoderRectangle : Decode.Decoder Rectangle
           decoderRectangle = decode Rectangle |> required "width" Decode.float |> required "height" Decode.float |> required "color" decoderColor
           """
      }

      "Shape" in {
        jsonDecoder[Elm](declaration[Shape]) shouldBe
          i"""
           decoderShape : Decode.Decoder Shape
           decoderShape = Decode.field "type" Decode.string |> Decode.andThen decoderShapeTpe

           decoderShapeTpe : String -> Decode.Decoder Shape
           decoderShapeTpe tpe =
              case tpe of
                 "Circle" -> decode Circle |> required "radius" Decode.float |> required "color" decoderColor
                 "Rectangle" -> decode Rectangle |> required "width" Decode.float |> required "height" Decode.float |> required "color" decoderColor
                 "ShapeGroup" -> decode ShapeGroup |> required "leftShape" decoderShape |> required "rightShape" decoderShape
                 _ -> Decode.fail ("Unexpected type for Shape: " ++ tpe)
           """
      }

      "Alpha" in {
        jsonDecoder[Elm](declaration[Alpha]) shouldBe
          i"""
           decoderAlpha : Decode.Decoder Alpha
           decoderAlpha = decode Alpha |> required "name" Decode.string |> required "char" Decode.string |> required "bool" Decode.bool
           """
      }

      "ArrayClass" in {
        jsonDecoder[Elm](declaration[ArrayClass]) shouldBe
          i"""
           decoderArrayClass : Decode.Decoder ArrayClass
           decoderArrayClass = decode ArrayClass |> required "aList" (Decode.list Decode.string) |> Decode.maybe (required "optField" Decode.float)
           """
      }

      "Numeric" in {
        jsonDecoder[Elm](declaration[Numeric]) shouldBe
          i"""
           decoderNumeric : Decode.Decoder Numeric
           decoderNumeric = decode Numeric |> required "double" Decode.float |> required "float" Decode.float |> required "int" Decode.int
           """
      }

      "ClassOrObject" in {
        jsonDecoder[Elm](declaration[ClassOrObject]) shouldBe
          i"""
           decoderClassOrObject : Decode.Decoder ClassOrObject
           decoderClassOrObject = Decode.field "type" Decode.string |> Decode.andThen decoderClassOrObjectTpe

           decoderClassOrObjectTpe : String -> Decode.Decoder ClassOrObject
           decoderClassOrObjectTpe tpe =
              case tpe of
                 "MyClass" -> decode MyClass |> required "value" Decode.int
                 "MyObject" -> Decode.succeed MyObject
                 _ -> Decode.fail ("Unexpected type for ClassOrObject: " ++ tpe)
           """
      }

      "Navigation" in {
        println(declaration[Navigation])
        jsonDecoder[Elm](declaration[Navigation]) shouldBe
          i"""
           decoderNavigation : Decode.Decoder Navigation
           decoderNavigation = Decode.field "type" Decode.string |> Decode.andThen decoderNavigationTpe

           decoderNavigationTpe : String -> Decode.Decoder Navigation
           decoderNavigationTpe tpe =
              case tpe of
                 "Node" -> decode Node |> required "name" Decode.string |> required "children" (Decode.list decoderNavigation)
                 "NodeList" -> decode NodeList |> required "all" (Decode.list decoderNavigation)
                 _ -> Decode.fail ("Unexpected type for Navigation: " ++ tpe)
           """
      }

      "ClassUUID" in {
        jsonDecoder[Elm](declaration[ClassUUID]) shouldBe
          i"""
           decoderClassUUID : Decode.Decoder ClassUUID
           decoderClassUUID = decode ClassUUID |> required "a" Uuid.decoder
           """
      }

      "ExternalReferences" in {
        jsonDecoder[Elm](declaration[ExternalReferences]) shouldBe
          i"""
           decoderExternalReferences : Decode.Decoder ExternalReferences
           decoderExternalReferences = decode ExternalReferences |> required "color" decoderColor |> required "nav" decoderNavigation
           """
      }

      "TypeOne and TypeTwo" in {
        jsonDecoder[Elm](List(declaration[TypeOne], declaration[TypeTwo])) shouldBe
          i"""
           decoderTypeOne : Decode.Decoder TypeOne
           decoderTypeOne = decode TypeOne |> required "name" Decode.string |> required "values" (Decode.list decoderTypeTwo)

           decoderTypeTwo : Decode.Decoder TypeTwo
           decoderTypeTwo = Decode.field "type" Decode.string |> Decode.andThen decoderTypeTwoTpe

           decoderTypeTwoTpe : String -> Decode.Decoder TypeTwo
           decoderTypeTwoTpe tpe =
              case tpe of
                 "OptionOne" -> decode OptionOne |> required "value" Decode.int
                 "OptionTwo" -> decode OptionTwo |> required "value" decoderTypeOne
                 _ -> Decode.fail ("Unexpected type for TypeTwo: " ++ tpe)
           """
      }

      "MyUUID" in {
        // we want to treat UUID as string, using an override
        implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
          Encoder.pure(Str)

        jsonDecoder[Elm](declaration[MyUUID]) shouldBe
          i"""
           decoderMyUUID : Decode.Decoder MyUUID
           decoderMyUUID = decode MyUUID |> required "uuid" Decode.string
           """
      }

      "ObjectsOnly" in {
        jsonDecoder[Elm](declaration[ObjectsOnly]) shouldBe
          i"""
           decoderObjectsOnly : Decode.Decoder ObjectsOnly
           decoderObjectsOnly = Decode.field "type" Decode.string |> Decode.andThen decoderObjectsOnlyTpe

           decoderObjectsOnlyTpe : String -> Decode.Decoder ObjectsOnly
           decoderObjectsOnlyTpe tpe =
              case tpe of
                 "ObjectOne" -> Decode.succeed ObjectOne
                 "ObjectTwo" -> Decode.succeed ObjectTwo
                 _ -> Decode.fail ("Unexpected type for ObjectsOnly: " ++ tpe)
           """
      }
    }

  }

}
