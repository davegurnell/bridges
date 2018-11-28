package bridges.typescript

import bridges.core.{ DeclF, Renderer }
import unindent._

object TsGuardRenderer
    extends TsGuardRenderer(
      predName = id => s"""is${id}""",
      guardName = id => s"""as${id}"""
    )

abstract class TsGuardRenderer(
    predName: String => String,
    guardName: String => String
) extends Renderer[TsType] {
  import TsType._
  import TsGuardExpr._

  def render(decl: TsDecl): String =
    i"""
    ${renderPred(decl)}

    ${renderGuard(decl)}
    """

  def renderPred(decl: TsDecl): String =
    i"""
    export function ${predName(decl.name)}(v: any): boolean {
      return ${TsGuardExpr.render(isType(ref("v"), decl.tpe))};
    }
    """

  def renderGuard(decl: TsDecl): String =
    i"""
    export function ${guardName(decl.name)}(v: any): ?${decl.name} {
      return ${predName(decl.name)}(v)
        ? v as ${decl.name}
        : throw new Error("Expected ${decl.name}, received " + JSON.stringify(v, null, 2));
    }
    """

  import TsGuardExpr._

  private def isType(expr: TsGuardExpr, tpe: TsType): TsGuardExpr =
    tpe match {
      case TsType.Ref(id)        => call(ref(predName(id)), expr)
      case TsType.Str            => eql(typeof(expr), lit("string"))
      case TsType.Chr            => eql(typeof(expr), lit("string"))
      case TsType.Intr           => eql(typeof(expr), lit("number"))
      case TsType.Real           => eql(typeof(expr), lit("number"))
      case TsType.Bool           => eql(typeof(expr), lit("boolean"))
      case TsType.StrLit(value)  => eql(expr, lit(value))
      case TsType.ChrLit(value)  => eql(expr, lit(value.toString))
      case TsType.IntrLit(value) => eql(expr, lit(value))
      case TsType.RealLit(value) => eql(expr, lit(value))
      case TsType.BoolLit(value) => eql(expr, lit(value))
      case TsType.Null           => eql(expr, nullLit)
      case TsType.Arr(tpe)       => isArray(expr, tpe)
      case TsType.Struct(fields) => isStruct(expr, fields)
      case TsType.Inter(types)   => isAll(expr, types)
      case TsType.Union(types)   => isUnion(expr, types)
    }

  private def isArray(expr: TsGuardExpr, tpe: TsType): TsGuardExpr =
    and(call(dot(ref("Array"), "isArray"), expr), call(dot(expr, "reduce"), func(List("a", "i"), and(ref("a"), isType(ref("i"), tpe)))))

  private def isStruct(expr: TsGuardExpr, fields: List[TsDecl]): TsGuardExpr =
    fields
      .map { case DeclF(name, tpe) => isType(dot(expr, name), tpe) }
      .reduceLeftOption(and)
      .getOrElse(lit(true))

  private def isUnion(expr: TsGuardExpr, types: List[TsType]): TsGuardExpr =
    types.collectAll { case tpe @ DiscriminatedBy(name, rest) => name -> rest } match {
      case Some(pairs) =>
        isDiscriminated(expr, pairs)
      case None =>
        isAny(expr, types)
    }

  private def isDiscriminated(expr: TsGuardExpr, types: List[(String, TsType.Struct)]): TsGuardExpr =
    types match {
      case Nil =>
        lit(false)

      case (name, head) :: tail =>
        cond(eql(dot(expr, "type"), lit(name)), isType(expr, head), isDiscriminated(expr, tail))
    }

  private def isAny(expr: TsGuardExpr, types: List[TsType]): TsGuardExpr =
    types
      .map(isType(expr, _))
      .reduceLeftOption(or)
      .getOrElse(lit(false))

  private def isAll(expr: TsGuardExpr, types: List[TsType]): TsGuardExpr =
    types
      .map(isType(expr, _))
      .reduceLeftOption(and)
      .getOrElse(lit(true))

  private implicit class ListOps[A](list: List[A]) {
    def collectAll[B](func: PartialFunction[A, B]): Option[List[B]] = {
      val temp = list.collect(func)
      if (temp.length == list.length) Some(temp) else None
    }
  }

  private object DiscriminatedBy {
    def unapply(tpe: TsType): Option[(String, TsType.Struct)] =
      tpe match {
        case TsType.Struct(fields) =>
          fields.collectFirst {
            case decl @ DeclF("type", TsType.StrLit(name)) =>
              (name, TsType.Struct(fields.filterNot(_ == decl)))
          }

        case _ =>
          None
      }
  }
}
