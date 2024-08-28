package bridges.core

import scala.compiletime._
import scala.deriving._

trait DerivedEncoderInstances extends DerivedEncoderInstances1 {
  // given valueClassEncoder[A <: AnyVal, B](implicit encoder: BasicEncoder[B]): BasicEncoder[A] =
  //   ???
}

trait DerivedEncoderInstances1 extends DerivedEncoderInstances0 {
  inline given allEncoders[A <: Tuple]: List[Encoder[Any]] = {
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (h *: t) => summonInline[Encoder[h]].asInstanceOf[Encoder[Any]] :: allEncoders[t]
    }
  }

  inline given allProdEncoders[A <: Tuple]: List[ProdEncoder[Any]] = {
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (h *: t) => summonInline[ProdEncoder[h]].asInstanceOf[ProdEncoder[Any]] :: allProdEncoders[t]
    }
  }

  private inline def allLabels[A <: Tuple]: List[String] = {
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (h *: t)  => constValue[h].asInstanceOf[String] :: allLabels[t]
    }
  }

  inline given deriveEncoder[A](using mirror: Mirror.Of[A]): Encoder[A] = {
    lazy val name = TypeName.getTypeName[A]

    println(s"derivedEncoder[${name}]")

    lazy val labels: List[String] =
      allLabels[mirror.MirroredElemLabels]

    inline mirror match {
      case mirror: Mirror.ProductOf[A] => prodEncoder(mirror, labels)
      case mirror: Mirror.SumOf[A]     => sumEncoder(mirror, labels)
    }
  }

  private inline def prodEncoder[A](mirror: Mirror.ProductOf[A], labels: List[String]): Encoder[A] = {
    lazy val types: List[Type] = allEncoders[mirror.MirroredElemTypes].map(_.encode)
    pureProd(Type.Prod(labels.zip(types)))
  }

  private inline def sumEncoder[A](mirror: Mirror.SumOf[A], labels: List[String]): Encoder[A] = {
    lazy val types: List[Type.Prod] = allProdEncoders[mirror.MirroredElemTypes].map(_.encode)
    pure(Type.Sum(labels.zip(types)))
  }
}

trait DerivedEncoderInstances0 extends EncoderConstructors {
  inline given genericBasicEncoder[A]: BasicEncoder[A] = {
    lazy val name = TypeName.getTypeName[A]
    println(s"genericBasicEncoder[${name}]")
    pure(Type.Ref(name))
  }
}
