package bridges.elm

import bridges.SampleTypes._
import bridges.core._
import bridges.core.Type._
import bridges.core.syntax._
import org.scalatest._
import unindent._

class ElmJsonEncoderSpec extends FreeSpec with Matchers {
  "Color" in {
    Elm.encoder(decl[Color]) shouldBe
    i"""
      encoderColor : Color -> Encode.Value
      encoderColor obj = Encode.object [ ("red", Encode.int obj.red), ("green", Encode.int obj.green), ("blue", Encode.int obj.blue) ]
      """
  }

  "Circle" in {
    Elm.encoder(decl[Circle]) shouldBe
    i"""
      encoderCircle : Circle -> Encode.Value
      encoderCircle obj = Encode.object [ ("radius", Encode.float obj.radius), ("color", encoderColor obj.color) ]
      """
  }

  "Rectangle" in {
    Elm.encoder(decl[Rectangle]) shouldBe
    i"""
      encoderRectangle : Rectangle -> Encode.Value
      encoderRectangle obj = Encode.object [ ("width", Encode.float obj.width), ("height", Encode.float obj.height), ("color", encoderColor obj.color) ]
      """
  }

  "Shape" in {
    Elm.encoder(decl[Shape]) shouldBe
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
    Elm.encoder(decl[Alpha]) shouldBe
    i"""
      encoderAlpha : Alpha -> Encode.Value
      encoderAlpha obj = Encode.object [ ("name", Encode.string obj.name), ("char", Encode.string obj.char), ("bool", Encode.bool obj.bool) ]
      """
  }

  "ArrayClass" in {
    Elm.encoder(decl[ArrayClass]) shouldBe
    i"""
      encoderArrayClass : ArrayClass -> Encode.Value
      encoderArrayClass obj = Encode.object [ ("aList", Encode.list (List.map Encode.string obj.aList)), ("optField", Maybe.withDefault Encode.null (Maybe.map Encode.float obj.optField)) ]
      """
  }

  "Numeric" in {
    Elm.encoder(decl[Numeric]) shouldBe
    i"""
      encoderNumeric : Numeric -> Encode.Value
      encoderNumeric obj = Encode.object [ ("double", Encode.float obj.double), ("float", Encode.float obj.float), ("int", Encode.int obj.int) ]
      """
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
    Elm.encoder(decl[Navigation]) shouldBe
    i"""
      encoderNavigation : Navigation -> Encode.Value
      encoderNavigation tpe =
         case tpe of
            Node name children -> Encode.object [ ("name", Encode.string name), ("children", Encode.list (List.map encoderNavigation children)), ("type", Encode.string "Node") ]
            NodeList all -> Encode.object [ ("all", Encode.list (List.map encoderNavigation all)), ("type", Encode.string "NodeList") ]
      """
  }

  "ExternalReferences" in {
    Elm.encoder(decl[ExternalReferences]) shouldBe
    i"""
      encoderExternalReferences : ExternalReferences -> Encode.Value
      encoderExternalReferences obj = Encode.object [ ("color", encoderColor obj.color), ("nav", encoderNavigation obj.nav) ]
      """
  }

  "TypeOne and TypeTwo" in {
    Elm.encoder(List(decl[TypeOne], decl[TypeTwo]), Map.empty[Ref, TypeReplacement]) shouldBe
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

  "ClassUUID" - {
    "by default we treat UUID as a normal type we created" in {
      // this is the case when we don't import any Elm specific UUID library and we will create our own UUID type there

      Elm.encoder(decl[ClassUUID]) shouldBe
      i"""
      encoderClassUUID : ClassUUID -> Encode.Value
      encoderClassUUID obj = Encode.object [ ("a", encoderUUID obj.a) ]
      """
    }

    "we can provide a map to substitute UUID decoding with a custom encoding logic" in {
      // this is the case when we import Elm specific UUID types we want to use in our decoder, but Scala can't know about them without extra hints
      val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
        Ref("UUID") → TypeReplacement("Uuid", "import Uuid exposing (Uuid)", "Uuid.decoder", "Uuid.encode")
      )

      Elm.encoder(decl[ClassUUID], customTypeReplacements) shouldBe
      i"""
      encoderClassUUID : ClassUUID -> Encode.Value
      encoderClassUUID obj = Encode.object [ ("a", Uuid.encode obj.a) ]
      """
    }

    "we can override the Encoder so we treat UUID as another basic type, like String, and encode it accordingly" in {
      // probably not recommended, better to use a mapping as in other tests, but it is supported
      implicit val uuidEncoder: BasicEncoder[java.util.UUID] =
        Encoder.pure(Str)

      Elm.encoder(decl[ClassUUID]) shouldBe
      i"""
      encoderClassUUID : ClassUUID -> Encode.Value
      encoderClassUUID obj = Encode.object [ ("a", Encode.string obj.a) ]
      """
    }
  }

  "ClassDate" - {
    "by default we treat Date as a normal type we created" in {
      // this is the case when we don't import any Elm specific Date library and we will create our own Date type there

      Elm.encoder(decl[ClassDate]) shouldBe
      i"""
      encoderClassDate : ClassDate -> Encode.Value
      encoderClassDate obj = Encode.object [ ("a", encoderDate obj.a) ]
      """
    }

    "we can provide a map to substitute Date decoding with a custom encoding logic" in {
      // this is the case when we import Elm specific Date types we want to use in our decoder, but Scala can't know about them without extra hints
      val customTypeReplacements: Map[Ref, TypeReplacement] = Map(
        Ref("Date") → TypeReplacement("Date", "import Date exposing (Date)", "Date.decoder", "Date.encode")
      )

      Elm.encoder(decl[ClassDate], customTypeReplacements) shouldBe
      i"""
      encoderClassDate : ClassDate -> Encode.Value
      encoderClassDate obj = Encode.object [ ("a", Date.encode obj.a) ]
      """
    }

    "we can override the Encoder so we treat Date as another basic type, like String, and encode it accordingly" in {
      // probably not recommended, better to use a mapping as in other tests, but it is supported
      implicit val dateEncoder: BasicEncoder[java.util.Date] =
        Encoder.pure(Str)

      Elm.encoder(decl[ClassDate]) shouldBe
      i"""
      encoderClassDate : ClassDate -> Encode.Value
      encoderClassDate obj = Encode.object [ ("a", Encode.string obj.a) ]
      """
    }
  }

  "ObjectsOnly" in {
    Elm.encoder(decl[ObjectsOnly]) shouldBe
    i"""
      encoderObjectsOnly : ObjectsOnly -> Encode.Value
      encoderObjectsOnly tpe =
         case tpe of
            ObjectOne -> Encode.object [ ("type", Encode.string "ObjectOne") ]
            ObjectTwo -> Encode.object [ ("type", Encode.string "ObjectTwo") ]
      """
  }
}
