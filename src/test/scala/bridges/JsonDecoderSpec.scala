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
           decoder : Decode.Decoder Color
           decoder = decode Color |> required "red" Decode.int |> required "green" Decode.int |> required "blue" Decode.int
           """
      }

      "Circle" in {
        jsonDecoder[Elm](declaration[Circle]) shouldBe
          i"""
           decoder : Decode.Decoder Circle
           decoder = decode Circle |> required "radius" Decode.float |> required "color" Color.decoder
           """
      }

      "Rectangle" in {
        jsonDecoder[Elm](declaration[Rectangle]) shouldBe
          i"""
           decoder : Decode.Decoder Rectangle
           decoder = decode Rectangle |> required "width" Decode.float |> required "height" Decode.float |> required "color" Color.decoder
           """
      }

      "Shape" in {
        jsonDecoder[Elm](declaration[Shape]) shouldBe
          i"""
           decoder : Decode.Decoder Shape
           decoder = Decode.field "type" Decode.string |> Decode.andThen decoderShape

           decoderShape : String -> Decode.Decoder Shape
           decoderShape tpe =
              case tpe of
                 "Circle" -> decode Circle |> required "radius" Decode.float |> required "color" Color.decoder
                 "Rectangle" -> decode Rectangle |> required "width" Decode.float |> required "height" Decode.float |> required "color" Color.decoder
                 "ShapeGroup" -> decode ShapeGroup |> required "leftShape" decoder |> required "rightShape" decoder
                 _ -> Decode.fail ("Unexpected type for Shape")
           """
      }

      "Alpha" in {
        jsonDecoder[Elm](declaration[Alpha]) shouldBe
          i"""
           decoder : Decode.Decoder Alpha
           decoder = decode Alpha |> required "name" Decode.string |> required "char" Decode.string |> required "bool" Decode.bool
           """
      }

      "ArrayClass" in {
        jsonDecoder[Elm](declaration[ArrayClass]) shouldBe
          i"""
           decoder : Decode.Decoder ArrayClass
           decoder = decode ArrayClass |> required "aList" (Decode.list Decode.string) |> Decode.maybe (required "optField" Decode.float)
           """
      }

      "Numeric" in {
        jsonDecoder[Elm](declaration[Numeric]) shouldBe
          i"""
           decoder : Decode.Decoder Numeric
           decoder = decode Numeric |> required "double" Decode.float |> required "float" Decode.float |> required "int" Decode.int
           """
      }

      "ClassOrObject" in {
        jsonDecoder[Elm](declaration[ClassOrObject]) shouldBe
          i"""
           decoder : Decode.Decoder ClassOrObject
           decoder = Decode.field "type" Decode.string |> Decode.andThen decoderClassOrObject

           decoderClassOrObject : String -> Decode.Decoder ClassOrObject
           decoderClassOrObject tpe =
              case tpe of
                 "MyClass" -> decode MyClass |> required "value" Decode.int
                 "MyObject" -> Decode.succeed MyObject
                 _ -> Decode.fail ("Unexpected type for ClassOrObject")
           """
      }

      "Navigation" in {
        println(declaration[Navigation])
        jsonDecoder[Elm](declaration[Navigation]) shouldBe
          i"""
           decoder : Decode.Decoder Navigation
           decoder = Decode.field "type" Decode.string |> Decode.andThen decoderNavigation

           decoderNavigation : String -> Decode.Decoder Navigation
           decoderNavigation tpe =
              case tpe of
                 "Node" -> decode Node |> required "name" Decode.string |> required "children" (Decode.list decoder)
                 "NodeList" -> decode NodeList |> required "all" (Decode.list decoder)
                 _ -> Decode.fail ("Unexpected type for Navigation")
           """
      }

      "ClassUUID" in {
        jsonDecoder[Elm](declaration[ClassUUID]) shouldBe
          i"""
           decoder : Decode.Decoder ClassUUID
           decoder = decode ClassUUID |> required "a" Uuid.decoder
           """
      }

      "MyUUID" in {
        // we want to treat UUID as string, using an override
        implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
          Encoder.pure(Str)

        jsonDecoder[Elm](declaration[MyUUID]) shouldBe
          i"""
           decoder : Decode.Decoder MyUUID
           decoder = decode MyUUID |> required "uuid" Decode.string
           """
      }
    }

  }

}
