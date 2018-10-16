package bridges.elm

import bridges.SampleTypes._
import bridges.core.Type._
import bridges.core._
import bridges.core.syntax._
import org.scalatest._
import unindent._

class ElmJsonDecoderSpec extends FreeSpec with Matchers {
  "decoder" - {
    "elm" - {

      "Color" in {
        Elm.decoder(decl[Color]) shouldBe
        i"""
           decoderColor : Decode.Decoder Color
           decoderColor = decode Color |> required "red" Decode.int |> required "green" Decode.int |> required "blue" Decode.int
           """
      }

      "Circle" in {
        Elm.decoder(decl[Circle]) shouldBe
        i"""
           decoderCircle : Decode.Decoder Circle
           decoderCircle = decode Circle |> required "radius" Decode.float |> required "color" (Decode.lazy (\\_ -> decoderColor))
           """
      }

      "Rectangle" in {
        Elm.decoder(decl[Rectangle]) shouldBe
        i"""
           decoderRectangle : Decode.Decoder Rectangle
           decoderRectangle = decode Rectangle |> required "width" Decode.float |> required "height" Decode.float |> required "color" (Decode.lazy (\\_ -> decoderColor))
           """
      }

      "Shape" in {
        Elm.decoder(decl[Shape]) shouldBe
        i"""
           decoderShape : Decode.Decoder Shape
           decoderShape = Decode.field "type" Decode.string |> Decode.andThen decoderShapeTpe

           decoderShapeTpe : String -> Decode.Decoder Shape
           decoderShapeTpe tpe =
              case tpe of
                 "Circle" -> decode Circle |> required "radius" Decode.float |> required "color" (Decode.lazy (\\_ -> decoderColor))
                 "Rectangle" -> decode Rectangle |> required "width" Decode.float |> required "height" Decode.float |> required "color" (Decode.lazy (\\_ -> decoderColor))
                 "ShapeGroup" -> decode ShapeGroup |> required "leftShape" (Decode.lazy (\\_ -> decoderShape)) |> required "rightShape" (Decode.lazy (\\_ -> decoderShape))
                 _ -> Decode.fail ("Unexpected type for Shape: " ++ tpe)
           """
      }

      "Alpha" in {
        Elm.decoder(decl[Alpha]) shouldBe
        i"""
           decoderAlpha : Decode.Decoder Alpha
           decoderAlpha = decode Alpha |> required "name" Decode.string |> required "char" Decode.string |> required "bool" Decode.bool
           """
      }

      "ArrayClass" in {
        Elm.decoder(decl[ArrayClass]) shouldBe
        i"""
           decoderArrayClass : Decode.Decoder ArrayClass
           decoderArrayClass = decode ArrayClass |> required "aList" (Decode.list Decode.string) |> optional "optField" (Decode.maybe Decode.float) Nothing
           """
      }

      "Numeric" in {
        Elm.decoder(decl[Numeric]) shouldBe
        i"""
           decoderNumeric : Decode.Decoder Numeric
           decoderNumeric = decode Numeric |> required "double" Decode.float |> required "float" Decode.float |> required "int" Decode.int
           """
      }

      "ClassOrObject" in {
        Elm.decoder(decl[ClassOrObject]) shouldBe
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
        Elm.decoder(decl[Navigation]) shouldBe
        i"""
           decoderNavigation : Decode.Decoder Navigation
           decoderNavigation = Decode.field "type" Decode.string |> Decode.andThen decoderNavigationTpe

           decoderNavigationTpe : String -> Decode.Decoder Navigation
           decoderNavigationTpe tpe =
              case tpe of
                 "Node" -> decode Node |> required "name" Decode.string |> required "children" (Decode.list (Decode.lazy (\\_ -> decoderNavigation)))
                 "NodeList" -> decode NodeList |> required "all" (Decode.list (Decode.lazy (\\_ -> decoderNavigation)))
                 _ -> Decode.fail ("Unexpected type for Navigation: " ++ tpe)
           """
      }

      "ExternalReferences" in {
        Elm.decoder(decl[ExternalReferences]) shouldBe
        i"""
           decoderExternalReferences : Decode.Decoder ExternalReferences
           decoderExternalReferences = decode ExternalReferences |> required "color" (Decode.lazy (\\_ -> decoderColor)) |> required "nav" (Decode.lazy (\\_ -> decoderNavigation))
           """
      }

      "TypeOne and TypeTwo" in {
        Elm.decoder(List(decl[TypeOne], decl[TypeTwo]), Map.empty[Ref, TypeReplacement]) shouldBe
        i"""
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
           """
      }

      "ClassUUID" - {
        "by default we treat UUID as a normal type we created" in {
          // this is the case when we don't import any Elm specific UUID library and we will create our own UUID type there

          Elm.decoder(decl[ClassUUID]) shouldBe
          i"""
           decoderClassUUID : Decode.Decoder ClassUUID
           decoderClassUUID = decode ClassUUID |> required "a" (Decode.lazy (\\_ -> decoderUUID))
           """
        }

        "we can provide a map to substitute UUID decoding with a custom decoding logic" in {
          // this is the case when we import Elm specific UUID types we want to use in our decoder, but Scala can't know about them without extra hints
          val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
            Ref("UUID") → TypeReplacement("Uuid", "import Uuid exposing (Uuid)", "Uuid.decoder", "Uuid.encode")
          )

          Elm.decoder(decl[ClassUUID], customTypeReplacements) shouldBe
          i"""
           decoderClassUUID : Decode.Decoder ClassUUID
           decoderClassUUID = decode ClassUUID |> required "a" Uuid.decoder
           """
        }

        "we can override the Encoder so we treat UUID as another basic type, like String, and decode it accordingly" in {
          // probably not recommended, better to use a mapping as in other tests, but it is supported
          implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
            Encoder.pure(Str)

          Elm.decoder(decl[ClassUUID]) shouldBe
          i"""
           decoderClassUUID : Decode.Decoder ClassUUID
           decoderClassUUID = decode ClassUUID |> required "a" Decode.string
           """
        }
      }

      "ClassDate" - {
        "by default we treat Date as a normal type we created" in {
          // this is the case when we don't import any Elm specific Date library and we will create our own Date type there

          Elm.decoder(decl[ClassDate]) shouldBe
          i"""
           decoderClassDate : Decode.Decoder ClassDate
           decoderClassDate = decode ClassDate |> required "a" (Decode.lazy (\\_ -> decoderDate))
           """
        }

        "we can provide a map to substitute Date decoding with a custom decoding logic" in {
          // this is the case when we import Elm specific UUID types we want to use in our decoder, but Scala can't know about them without extra hints
          val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
            Ref("Date") → TypeReplacement("Date", "import Date exposing (Date)", "Date.decoder", "Date.encode")
          )

          Elm.decoder(decl[ClassDate], customTypeReplacements) shouldBe
          i"""
           decoderClassDate : Decode.Decoder ClassDate
           decoderClassDate = decode ClassDate |> required "a" Date.decoder
           """
        }

        "we can override the Encoder so we treat Date as another basic type, like String, and decode it accordingly" in {
          // probably not recommended, better to use a mapping as in other tests, but it is supported
          implicit val dateEncoder: BasicEncoder[java.util.Date] =
            Encoder.pure(Str)

          Elm.decoder(decl[ClassDate]) shouldBe
          i"""
           decoderClassDate : Decode.Decoder ClassDate
           decoderClassDate = decode ClassDate |> required "a" Decode.string
           """
        }
      }

      "ObjectsOnly" in {
        Elm.decoder(decl[ObjectsOnly]) shouldBe
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
