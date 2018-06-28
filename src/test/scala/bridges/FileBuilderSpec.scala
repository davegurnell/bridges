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
           import Uuid exposing (Uuid)


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
           import Uuid exposing (Uuid)
           import CustomModule.Color as Color exposing (Color)

           type Shape = Circle Float Color | Rectangle Float Float Color | ShapeGroup Shape Shape

           decoder : Decode.Decoder Shape
           decoder = Decode.field "type" Decode.string |> Decode.andThen decoderShape

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

      "for a recursive trait" in {
        val fileContent =
          i"""
           module CustomModule.Navigation exposing (Navigation, decoder, encoder)

           import Json.Decode as Decode
           import Json.Decode.Pipeline exposing (..)
           import Json.Encode as Encode
           import Uuid exposing (Uuid)


           type Navigation = Node String (List Navigation) | NodeList (List Navigation)

           decoder : Decode.Decoder Navigation
           decoder = Decode.field "type" Decode.string |> Decode.andThen decoderNavigation

           decoderNavigation : String -> Decode.Decoder Navigation
           decoderNavigation tpe =
              case tpe of
                 "Node" -> decode Node |> required "name" Decode.string |> required "children" (Decode.list Navigation.decoder)
                 "NodeList" -> decode NodeList |> required "all" (Decode.list Navigation.decoder)
                 _ -> Decode.fail ("Unexpected type for Navigation")

           encoder : Navigation -> Encode.Value
           encoder tpe =
              case tpe of
                 Node name children -> Encode.object [ ("name", Encode.string name), ("children", Encode.list (List.map Navigation.encoder children)), ("type", Encode.string "Node") ]
                 NodeList all -> Encode.object [ ("all", Encode.list (List.map Navigation.encoder all)), ("type", Encode.string "NodeList") ]
           """
        val expected = Map("Navigation.elm" → fileContent)

        buildFile[Elm]("CustomModule", declaration[Navigation]) shouldBe expected
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
           import Uuid exposing (Uuid)


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
           import Uuid exposing (Uuid)


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
           import Uuid exposing (Uuid)


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
