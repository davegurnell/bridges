package bridges

import bridges.SampleTypes._
import bridges.Type.Str
import bridges.syntax._
import org.scalatest._
import unindent._

class FileBuilderSpec extends FreeSpec with Matchers {

  "buildFile" - {
    "elm" - {

      "for a single case class" in {
        val fileContent =
          i"""
           module CustomModule.Color exposing (Color, decoder, encoder)

           import Json.Decode as Decode
           import Json.Decode.Pipeline exposing (..)
           import Json.Encode as Encode

           type alias Color = { red: Int, green: Int, blue: Int }

           decoder : Decode.Decoder Color
           decoder = decode Color |> required "red" Decode.int |> required "green" Decode.int |> required "blue" Decode.int

           encoder : Color -> Encode.Value
           encoder obj = Encode.object [ ("red", Encode.int obj.red), ("green", Encode.int obj.green), ("blue", Encode.int obj.blue) ]
           """
        val expected = Map("Color.elm" → fileContent)

        buildFile[Elm]("CustomModule", declaration[Color]) shouldBe expected
      }

      "for a trait" in {
        val fileContent =
          i"""
           module CustomModule.Shape exposing (Shape, decoder, encoder)

           import Json.Decode as Decode
           import Json.Decode.Pipeline exposing (..)
           import Json.Encode as Encode

           type Shape = Circle Float Color | Rectangle Float Float Color | ShapeGroup Shape Shape

           decoder : Decode.Decoder Shape
           decoder = field "type" string |> Decode.andThen decoderShape

           decoderShape : String -> Decode.Decoder Shape
           decoderShape tpe =
              case tpe of
                 "Circle" -> decode Circle |> required "radius" Decode.float |> required "color" Color.decoder
                 "Rectangle" -> decode Rectangle |> required "width" Decode.float |> required "height" Decode.float |> required "color" Color.decoder
                 "ShapeGroup" -> decode ShapeGroup |> required "leftShape" Shape.decoder |> required "rightShape" Shape.decoder
                 _ -> Decode.fail ("Unexpected type for Shape")

           encoder : Shape -> Encode.Value
           encoder tpe =
              case tpe of
                 Circle radius color -> Encode.object [ ("radius", Encode.float radius), ("color", Color.encoder color), ("type", Encode.string "Circle") ]
                 Rectangle width height color -> Encode.object [ ("width", Encode.float width), ("height", Encode.float height), ("color", Color.encoder color), ("type", Encode.string "Rectangle") ]
                 ShapeGroup leftShape rightShape -> Encode.object [ ("leftShape", Shape.encoder leftShape), ("rightShape", Shape.encoder rightShape), ("type", Encode.string "ShapeGroup") ]
           """
        val expected = Map("Shape.elm" → fileContent)

        buildFile[Elm]("CustomModule", declaration[Shape]) shouldBe expected
      }

      "with overrides" in {
        // we want to treat UUID as string, using an override
        implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
          Encoder.pure(Str)

        val fileContent =
          i"""
           module CustomModule2.MyUUID exposing (MyUUID, decoder, encoder)

           import Json.Decode as Decode
           import Json.Decode.Pipeline exposing (..)
           import Json.Encode as Encode

           type alias MyUUID = { uuid: String }

           decoder : Decode.Decoder MyUUID
           decoder = decode MyUUID |> required "uuid" Decode.string

           encoder : MyUUID -> Encode.Value
           encoder obj = Encode.object [ ("uuid", Encode.string obj.uuid) ]
           """
        val expected = Map("MyUUID.elm" → fileContent)

        buildFile[Elm]("CustomModule2", declaration[MyUUID]) shouldBe expected
      }

      "for several classes at once" in {
        // we want to treat UUID as string, using an override
        implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
          Encoder.pure(Str)

        val fileContentColor =
          i"""
           module CustomModule2.Color exposing (Color, decoder, encoder)

           import Json.Decode as Decode
           import Json.Decode.Pipeline exposing (..)
           import Json.Encode as Encode

           type alias Color = { red: Int, green: Int, blue: Int }

           decoder : Decode.Decoder Color
           decoder = decode Color |> required "red" Decode.int |> required "green" Decode.int |> required "blue" Decode.int

           encoder : Color -> Encode.Value
           encoder obj = Encode.object [ ("red", Encode.int obj.red), ("green", Encode.int obj.green), ("blue", Encode.int obj.blue) ]
           """

        val fileContentUuid =
          i"""
           module CustomModule2.MyUUID exposing (MyUUID, decoder, encoder)

           import Json.Decode as Decode
           import Json.Decode.Pipeline exposing (..)
           import Json.Encode as Encode

           type alias MyUUID = { uuid: String }

           decoder : Decode.Decoder MyUUID
           decoder = decode MyUUID |> required "uuid" Decode.string

           encoder : MyUUID -> Encode.Value
           encoder obj = Encode.object [ ("uuid", Encode.string obj.uuid) ]
           """
        val expected =
          Map("Color.elm" → fileContentColor, "MyUUID.elm" → fileContentUuid)

        buildFile[Elm](
          "CustomModule2",
          List(declaration[Color], declaration[MyUUID])
        ) shouldBe expected
      }
    }
  }

}
