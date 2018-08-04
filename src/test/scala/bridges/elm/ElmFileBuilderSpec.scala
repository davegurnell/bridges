package bridges.elm

import bridges.SampleTypes._
import bridges.core.Type._
import bridges.syntax._
import org.scalatest._
import unindent._

class ElmFileBuilderSpec extends FreeSpec with Matchers {
  "for a single case class" in {
    val fileContent =
      i"""
      module CustomModule.Color exposing (..)

      import Json.Decode as Decode
      import Json.Decode.Pipeline exposing (..)
      import Json.Encode as Encode


      type alias Color = { red: Int, green: Int, blue: Int }


      decoderColor : Decode.Decoder Color
      decoderColor = decode Color |> required "red" Decode.int |> required "green" Decode.int |> required "blue" Decode.int


      encoderColor : Color -> Encode.Value
      encoderColor obj = Encode.object [ ("red", Encode.int obj.red), ("green", Encode.int obj.green), ("blue", Encode.int obj.blue) ]
      """
    val expected = ("Color.elm", fileContent)

    Elm.buildFile("CustomModule", declaration[Color]) shouldBe expected
  }

  "for a single case class with complex types" in {
    val fileContent =
      i"""
      module CustomModule.ExternalReferences exposing (..)

      import Json.Decode as Decode
      import Json.Decode.Pipeline exposing (..)
      import Json.Encode as Encode
      import CustomModule.Color exposing (..)
      import CustomModule.Navigation exposing (..)

      type alias ExternalReferences = { color: Color, nav: Navigation }


      decoderExternalReferences : Decode.Decoder ExternalReferences
      decoderExternalReferences = decode ExternalReferences |> required "color" (Decode.lazy (\\_ -> decoderColor)) |> required "nav" (Decode.lazy (\\_ -> decoderNavigation))


      encoderExternalReferences : ExternalReferences -> Encode.Value
      encoderExternalReferences obj = Encode.object [ ("color", encoderColor obj.color), ("nav", encoderNavigation obj.nav) ]
      """
    val expected = ("ExternalReferences.elm", fileContent)

    Elm.buildFile("CustomModule", declaration[ExternalReferences]) shouldBe expected
  }

  "for a trait" in {
    val fileContent =
      i"""
      module CustomModule.Shape exposing (..)

      import Json.Decode as Decode
      import Json.Decode.Pipeline exposing (..)
      import Json.Encode as Encode
      import CustomModule.Color exposing (..)

      type Shape = Circle Float Color | Rectangle Float Float Color | ShapeGroup Shape Shape


      decoderShape : Decode.Decoder Shape
      decoderShape = Decode.field "type" Decode.string |> Decode.andThen decoderShapeTpe

      decoderShapeTpe : String -> Decode.Decoder Shape
      decoderShapeTpe tpe =
         case tpe of
            "Circle" -> decode Circle |> required "radius" Decode.float |> required "color" (Decode.lazy (\\_ -> decoderColor))
            "Rectangle" -> decode Rectangle |> required "width" Decode.float |> required "height" Decode.float |> required "color" (Decode.lazy (\\_ -> decoderColor))
            "ShapeGroup" -> decode ShapeGroup |> required "leftShape" (Decode.lazy (\\_ -> decoderShape)) |> required "rightShape" (Decode.lazy (\\_ -> decoderShape))
            _ -> Decode.fail ("Unexpected type for Shape: " ++ tpe)


      encoderShape : Shape -> Encode.Value
      encoderShape tpe =
         case tpe of
            Circle radius color -> Encode.object [ ("radius", Encode.float radius), ("color", encoderColor color), ("type", Encode.string "Circle") ]
            Rectangle width height color -> Encode.object [ ("width", Encode.float width), ("height", Encode.float height), ("color", encoderColor color), ("type", Encode.string "Rectangle") ]
            ShapeGroup leftShape rightShape -> Encode.object [ ("leftShape", encoderShape leftShape), ("rightShape", encoderShape rightShape), ("type", Encode.string "ShapeGroup") ]
      """
    val expected = ("Shape.elm", fileContent)

    Elm.buildFile("CustomModule", declaration[Shape]) shouldBe expected
  }

  "for a recursive trait" in {
    val fileContent =
      i"""
      module CustomModule.Navigation exposing (..)

      import Json.Decode as Decode
      import Json.Decode.Pipeline exposing (..)
      import Json.Encode as Encode


      type Navigation = Node String (List Navigation) | NodeList (List Navigation)


      decoderNavigation : Decode.Decoder Navigation
      decoderNavigation = Decode.field "type" Decode.string |> Decode.andThen decoderNavigationTpe

      decoderNavigationTpe : String -> Decode.Decoder Navigation
      decoderNavigationTpe tpe =
         case tpe of
            "Node" -> decode Node |> required "name" Decode.string |> required "children" (Decode.list (Decode.lazy (\\_ -> decoderNavigation)))
            "NodeList" -> decode NodeList |> required "all" (Decode.list (Decode.lazy (\\_ -> decoderNavigation)))
            _ -> Decode.fail ("Unexpected type for Navigation: " ++ tpe)


      encoderNavigation : Navigation -> Encode.Value
      encoderNavigation tpe =
         case tpe of
            Node name children -> Encode.object [ ("name", Encode.string name), ("children", Encode.list (List.map encoderNavigation children)), ("type", Encode.string "Node") ]
            NodeList all -> Encode.object [ ("all", Encode.list (List.map encoderNavigation all)), ("type", Encode.string "NodeList") ]
      """
    val expected = ("Navigation.elm", fileContent)

    Elm.buildFile("CustomModule", declaration[Navigation]) shouldBe expected
  }

  "UUID" - {
    "without overrides" in {
      val fileContent =
        i"""
      module CustomModule2.ClassUUID exposing (..)

      import Json.Decode as Decode
      import Json.Decode.Pipeline exposing (..)
      import Json.Encode as Encode
      import CustomModule2.UUID exposing (..)

      type alias ClassUUID = { a: UUID }


      decoderClassUUID : Decode.Decoder ClassUUID
      decoderClassUUID = decode ClassUUID |> required "a" (Decode.lazy (\\_ -> decoderUUID))


      encoderClassUUID : ClassUUID -> Encode.Value
      encoderClassUUID obj = Encode.object [ ("a", encoderUUID obj.a) ]
      """
      val expected = ("ClassUUID.elm", fileContent)

      Elm.buildFile("CustomModule2", declaration[ClassUUID]) shouldBe expected
    }

    "with overrides" in {
      val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
        Ref("UUID") → TypeReplacement("Uuid", "import Uuid exposing (Uuid)", "Uuid.decoder", "Uuid.encode")
      )

      val fileContent =
        i"""
      module CustomModule2.ClassUUID exposing (..)

      import Json.Decode as Decode
      import Json.Decode.Pipeline exposing (..)
      import Json.Encode as Encode
      import Uuid exposing (Uuid)

      type alias ClassUUID = { a: Uuid }


      decoderClassUUID : Decode.Decoder ClassUUID
      decoderClassUUID = decode ClassUUID |> required "a" Uuid.decoder


      encoderClassUUID : ClassUUID -> Encode.Value
      encoderClassUUID obj = Encode.object [ ("a", Uuid.encode obj.a) ]
      """
      val expected = ("ClassUUID.elm", fileContent)

      Elm.buildFile("CustomModule2", declaration[ClassUUID], customTypeReplacements) shouldBe expected
    }
  }

  "objects only" in {
    val fileContent =
      i"""
      module CustomModule2.ObjectsOnly exposing (..)

      import Json.Decode as Decode

      import Json.Encode as Encode


      type ObjectsOnly = ObjectOne | ObjectTwo


      decoderObjectsOnly : Decode.Decoder ObjectsOnly
      decoderObjectsOnly = Decode.field "type" Decode.string |> Decode.andThen decoderObjectsOnlyTpe

      decoderObjectsOnlyTpe : String -> Decode.Decoder ObjectsOnly
      decoderObjectsOnlyTpe tpe =
         case tpe of
            "ObjectOne" -> Decode.succeed ObjectOne
            "ObjectTwo" -> Decode.succeed ObjectTwo
            _ -> Decode.fail ("Unexpected type for ObjectsOnly: " ++ tpe)


      encoderObjectsOnly : ObjectsOnly -> Encode.Value
      encoderObjectsOnly tpe =
         case tpe of
            ObjectOne -> Encode.object [ ("type", Encode.string "ObjectOne") ]
            ObjectTwo -> Encode.object [ ("type", Encode.string "ObjectTwo") ]
       """
    val expected = ("ObjectsOnly.elm", fileContent)

    Elm.buildFile("CustomModule2", declaration[ObjectsOnly]) shouldBe expected
  }

  "for several classes at once" - {
    "without overrides" in {
      val fileContent =
        i"""
      module CustomModule2.Color exposing (..)

      import Json.Decode as Decode
      import Json.Decode.Pipeline exposing (..)
      import Json.Encode as Encode
      import CustomModule2.UUID exposing (..)

      type alias Color = { red: Int, green: Int, blue: Int }
      type alias ClassUUID = { a: UUID }


      decoderColor : Decode.Decoder Color
      decoderColor = decode Color |> required "red" Decode.int |> required "green" Decode.int |> required "blue" Decode.int

      decoderClassUUID : Decode.Decoder ClassUUID
      decoderClassUUID = decode ClassUUID |> required "a" (Decode.lazy (\\_ -> decoderUUID))


      encoderColor : Color -> Encode.Value
      encoderColor obj = Encode.object [ ("red", Encode.int obj.red), ("green", Encode.int obj.green), ("blue", Encode.int obj.blue) ]

      encoderClassUUID : ClassUUID -> Encode.Value
      encoderClassUUID obj = Encode.object [ ("a", encoderUUID obj.a) ]
      """

      val expected = ("Color.elm", fileContent)

      Elm.buildFile(
        "CustomModule2",
        List(declaration[Color], declaration[ClassUUID]),
        Map.empty[Ref, TypeReplacement]
      ) shouldBe expected
    }

    "with custom mappings" in {

      val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
        Ref("UUID") → TypeReplacement("Uuid", "import Uuid exposing (Uuid)", "Uuid.decoder", "Uuid.encode")
      )

      val fileContent =
        i"""
      module CustomModule2.Color exposing (..)

      import Json.Decode as Decode
      import Json.Decode.Pipeline exposing (..)
      import Json.Encode as Encode
      import Uuid exposing (Uuid)

      type alias Color = { red: Int, green: Int, blue: Int }
      type alias ClassUUID = { a: Uuid }


      decoderColor : Decode.Decoder Color
      decoderColor = decode Color |> required "red" Decode.int |> required "green" Decode.int |> required "blue" Decode.int

      decoderClassUUID : Decode.Decoder ClassUUID
      decoderClassUUID = decode ClassUUID |> required "a" Uuid.decoder


      encoderColor : Color -> Encode.Value
      encoderColor obj = Encode.object [ ("red", Encode.int obj.red), ("green", Encode.int obj.green), ("blue", Encode.int obj.blue) ]

      encoderClassUUID : ClassUUID -> Encode.Value
      encoderClassUUID obj = Encode.object [ ("a", Uuid.encode obj.a) ]
      """

      val expected = ("Color.elm", fileContent)

      Elm.buildFile(
        "CustomModule2",
        List(declaration[Color], declaration[ClassUUID]),
        customTypeReplacements
      ) shouldBe expected
    }
  }

  "for mutually recursive classes" in {
    val fileContent =
      i"""
      module CustomModule2.TypeOne exposing (..)

      import Json.Decode as Decode
      import Json.Decode.Pipeline exposing (..)
      import Json.Encode as Encode


      type alias TypeOne = { name: String, values: (List TypeTwo) }
      type TypeTwo = OptionOne Int | OptionTwo TypeOne


      decoderTypeOne : Decode.Decoder TypeOne
      decoderTypeOne = decode TypeOne |> required "name" Decode.string |> required "values" (Decode.list (Decode.lazy (\\_ -> decoderTypeTwo)))

      decoderTypeTwo : Decode.Decoder TypeTwo
      decoderTypeTwo = Decode.field "type" Decode.string |> Decode.andThen decoderTypeTwoTpe

      decoderTypeTwoTpe : String -> Decode.Decoder TypeTwo
      decoderTypeTwoTpe tpe =
         case tpe of
            "OptionOne" -> decode OptionOne |> required "value" Decode.int
            "OptionTwo" -> decode OptionTwo |> required "value" (Decode.lazy (\\_ -> decoderTypeOne))
            _ -> Decode.fail ("Unexpected type for TypeTwo: " ++ tpe)


      encoderTypeOne : TypeOne -> Encode.Value
      encoderTypeOne obj = Encode.object [ ("name", Encode.string obj.name), ("values", Encode.list (List.map encoderTypeTwo obj.values)) ]

      encoderTypeTwo : TypeTwo -> Encode.Value
      encoderTypeTwo tpe =
         case tpe of
            OptionOne value -> Encode.object [ ("value", Encode.int value), ("type", Encode.string "OptionOne") ]
            OptionTwo value -> Encode.object [ ("value", encoderTypeOne value), ("type", Encode.string "OptionTwo") ]
      """

    val expected = ("TypeOne.elm", fileContent)

    Elm.buildFile(
      "CustomModule2",
      List(declaration[TypeOne], declaration[TypeTwo]),
      Map.empty[Ref, TypeReplacement]
    ) shouldBe expected
  }
}
