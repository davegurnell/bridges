package bridges.typescript

import bridges.core.{ DeclF, Renderer }
import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }
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
  import TsExpr._

  def render(decl: TsDecl): String =
    i"""
    ${renderPred(decl)}

    ${renderGuard(decl)}
    """

  def renderPred(decl: TsDecl): String =
    i"""
    export function ${predName(decl.name)}(v: any): boolean {
      return ${TsExpr.render(isType(ref("v"), decl.tpe))};
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

  import TsExpr._

  private def isType(expr: TsExpr, tpe: TsType): TsExpr =
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

  private def isArray(expr: TsExpr, tpe: TsType): TsExpr =
    and(call(dot(ref("Array"), "isArray"), expr), call(dot(expr, "reduce"), func(List("a", "i"), and(ref("a"), isType(ref("i"), tpe)))))

  private def isStruct(expr: TsExpr, fields: List[TsDecl]): TsExpr =
    fields
      .map { case DeclF(name, tpe) => isType(dot(expr, name), tpe) }
      .reduceLeftOption(and)
      .getOrElse(lit(true))

  private def isUnion(expr: TsExpr, types: List[TsType]): TsExpr =
    types.collectAll { case tpe @ DiscriminatedBy(name, rest) => name -> rest } match {
      case Some(pairs) =>
        isDiscriminated(expr, pairs)
      case None =>
        isAny(expr, types)
    }

  private def isDiscriminated(expr: TsExpr, types: List[(String, TsType.Struct)]): TsExpr =
    types match {
      case Nil =>
        lit(false)

      case (name, head) :: tail =>
        cond(eql(dot(expr, "type"), lit(name)), isType(expr, head), isDiscriminated(expr, tail))
    }

  private def isAny(expr: TsExpr, types: List[TsType]): TsExpr =
    types
      .map(isType(expr, _))
      .reduceLeftOption(or)
      .getOrElse(lit(false))

  private def isAll(expr: TsExpr, types: List[TsType]): TsExpr =
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

sealed abstract class TsExpr

object TsExpr {
  final case class Ref(name: String)                                     extends TsExpr
  final case class Dot(obj: TsExpr, name: String)                        extends TsExpr
  final case class Lit(name: String)                                     extends TsExpr
  final case class Typeof(expr: TsExpr)                                  extends TsExpr
  final case class Call(func: TsExpr, args: List[TsExpr])                extends TsExpr
  final case class Func(args: List[String], body: TsExpr)                extends TsExpr
  final case class Cond(test: TsExpr, trueArm: TsExpr, falseArm: TsExpr) extends TsExpr
  final case class And(lhs: TsExpr, rhs: TsExpr)                         extends TsExpr
  final case class Or(lhs: TsExpr, rhs: TsExpr)                          extends TsExpr
  final case class Eql(lhs: TsExpr, rhs: TsExpr)                         extends TsExpr

  def ref(name: String): TsExpr =
    Ref(name)

  def dot(expr: TsExpr, name: String): TsExpr =
    Dot(expr, name)

  def lit(value: String): TsExpr =
    Lit(s""""${escape(value)}"""")

  def lit(value: Char): TsExpr =
    Lit(s""""${escape(value.toString)}"""")

  def lit(value: Int): TsExpr =
    Lit(value.toString)

  def lit(value: Double): TsExpr =
    Lit(value.toString)

  def lit(value: Boolean): TsExpr =
    Lit(value.toString)

  val nullLit: TsExpr =
    Lit("null")

  def typeof(lhs: TsExpr): TsExpr =
    Typeof(lhs)

  def call(func: TsExpr, args: TsExpr*): TsExpr =
    Call(func, args.toList)

  def func(args: List[String], body: TsExpr): TsExpr =
    Func(args, body)

  def cond(test: TsExpr, trueArm: TsExpr, falseArm: TsExpr): TsExpr =
    Cond(test, trueArm, falseArm)

  def and(lhs: TsExpr, rhs: TsExpr): TsExpr =
    And(lhs, rhs)

  def or(lhs: TsExpr, rhs: TsExpr): TsExpr =
    Or(lhs, rhs)

  def eql(lhs: TsExpr, rhs: TsExpr): TsExpr =
    Eql(lhs, rhs)

  def render(expr: TsExpr): String = {
    val r = renderParens(expr) _

    expr match {
      case Ref(name)        => name
      case Dot(obj, name)   => s"""${r(obj)}.${name}"""
      case Lit(value)       => value
      case Typeof(expr)     => s"""typeof ${r(expr)}"""
      case Call(func, args) => s"""${r(func)}(${args.map(render).mkString(", ")})"""
      case Func(args, body) => s"""(${args.mkString(", ")}) -> ${r(body)}"""
      case Cond(c, t, f)    => s"""${r(c)} ? ${r(t)} : ${r(f)}"""
      case And(lhs, rhs)    => s"""${r(lhs)} && ${r(rhs)}"""
      case Or(lhs, rhs)     => s"""${r(lhs)} || ${r(rhs)}"""
      case Eql(lhs, rhs)    => s"""${r(lhs)} === ${r(rhs)}"""
    }
  }

  private def renderParens(outer: TsExpr)(inner: TsExpr): String =
    if (precedence(outer) > precedence(inner)) {
      s"(${render(inner)})"
    } else {
      render(inner)
    }

  private def precedence(tpe: TsExpr): Int =
    tpe match {
      case _: Ref    => 1000
      case _: Dot    => 1000
      case _: Lit    => 1000
      case _: Call   => 1000
      case _: Func   => 1000
      case _: Typeof => 900
      case _: Eql    => 700
      case _: And    => 600
      case _: Or     => 500
      case _: Cond   => 400
    }
}
