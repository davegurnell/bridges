package bridges

import scala.compiletime.*
import scala.quoted.*

trait RefEncoder[A]:
  def encode: TsType

  override def toString: String =
    s"RefEncoder(${encode.toString})"

object RefEncoder extends RefEncoderInstances2:
  def apply[A](using encoder: RefEncoder[A]): RefEncoder[A] =
    encoder

  def encode[A](using encoder: RefEncoder[A]): TsType =
    encoder.encode

trait RefEncoderInstances2 extends RefEncoderInstances1:
  given fromTsEncoder[A: TsEncoder]: RefEncoder[A] =
    instance(TsEncoder[A].encode)
  //
  // given optionEncoder[A](using aEnc: RefEncoder[A]): RefEncoder[Option[A]] =
  //   instance(TsType.Union(List(aEnc.encode, TsType.Null)))
  //
  // given mapRefEncoder[A, B](using aEnc: RefEncoder[A], bEnc: RefEncoder[B]): RefEncoder[Map[A, B]] =
  //   instance(TsType.Record(aEnc.encode, bEnc.encode))
  //
  // given traversableEncoder[F[x] <: Iterable[x], A](using enc: RefEncoder[A]): RefEncoder[F[A]] =
  //   instance(TsType.Arr(enc.encode))

trait RefEncoderInstances1 extends RefEncoderConstructors:
  inline def summonOrDeriveAll[T <: Tuple]: List[RefEncoder[?]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonOrDerive[t] :: summonOrDeriveAll[ts]
    }

  private inline def summonOrDerive[A]: RefEncoder[A] =
    ${ RefEncoderMacros.summonOrDeriveImpl[A] }

  inline given derived[A]: RefEncoder[A] =
    ${ RefEncoderMacros.derivedImpl[A] }

trait RefEncoderConstructors:
  def instance[A](tpe: => TsType): RefEncoder[A] =
    new RefEncoder[A]:
      def encode: TsType = tpe

object RefEncoderMacros:
  def summonOrDeriveImpl[A: Type](using Quotes): Expr[RefEncoder[A]] =
    Expr
      .summon[RefEncoder[A]]
      .getOrElse(derivedImpl[A])

  def derivedImpl[A: Type](using Quotes): Expr[RefEncoder[A]] = {
    import quotes.reflect.*

    val tpe =
      TypeRepr.of[A]

    val sym =
      if tpe.isSingleton then
        if tpe.termSymbol == Symbol.noSymbol then
          tpe.typeSymbol
        else
          tpe.termSymbol
      else
        tpe.typeSymbol

    val name =
      sym.name.filterNot(_ == '$')

    '{ RefEncoder.instance(TsType.Ref(${ Expr(name) })) }
  }
