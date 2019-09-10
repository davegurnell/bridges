package bridges.typescript

import org.apache.commons.lang3.StringEscapeUtils.{ escapeJava => escape }

sealed abstract class TsGuardExpr

object TsGuardExpr {
  final case class Ref(name: String)                                                    extends TsGuardExpr
  final case class Dot(obj: TsGuardExpr, name: String)                                  extends TsGuardExpr
  final case class Arr(exprs: List[TsGuardExpr])                                        extends TsGuardExpr
  final case class Index(arr: TsGuardExpr, index: TsGuardExpr)                          extends TsGuardExpr
  final case class Lit(name: String)                                                    extends TsGuardExpr
  final case class Typeof(expr: TsGuardExpr)                                            extends TsGuardExpr
  final case class Call(func: TsGuardExpr, args: List[TsGuardExpr])                     extends TsGuardExpr
  final case class Func(args: List[String], body: TsGuardExpr)                          extends TsGuardExpr
  final case class Guard(arg: String, retType: TsType, body: TsGuardExpr)               extends TsGuardExpr
  final case class Cond(test: TsGuardExpr, trueArm: TsGuardExpr, falseArm: TsGuardExpr) extends TsGuardExpr
  final case class IsNull(expr: TsGuardExpr)                                            extends TsGuardExpr
  final case class Not(expr: TsGuardExpr)                                               extends TsGuardExpr
  final case class And(lhs: TsGuardExpr, rhs: TsGuardExpr)                              extends TsGuardExpr
  final case class Or(lhs: TsGuardExpr, rhs: TsGuardExpr)                               extends TsGuardExpr
  final case class Eql(lhs: TsGuardExpr, rhs: TsGuardExpr)                              extends TsGuardExpr
  final case class In(key: String, expr: TsGuardExpr)                                   extends TsGuardExpr

  def ref(name: String): TsGuardExpr =
    Ref(name)

  def dot(expr: TsGuardExpr, name: String): TsGuardExpr =
    Dot(expr, name)

  def arr(exprs: List[TsGuardExpr]): TsGuardExpr =
    Arr(exprs)

  def index(expr: TsGuardExpr, index: Int): TsGuardExpr =
    Index(expr, lit(index))

  def index(expr: TsGuardExpr, index: TsGuardExpr): TsGuardExpr =
    Index(expr, index)

  def lit(value: String): TsGuardExpr =
    Lit(s""""${escape(value)}"""")

  def lit(value: Char): TsGuardExpr =
    Lit(s""""${escape(value.toString)}"""")

  def lit(value: Int): TsGuardExpr =
    Lit(value.toString)

  def lit(value: Double): TsGuardExpr =
    Lit(value.toString)

  def lit(value: Boolean): TsGuardExpr =
    Lit(value.toString)

  val nullLit: TsGuardExpr =
    Lit("null")

  def typeof(lhs: TsGuardExpr): TsGuardExpr =
    Typeof(lhs)

  def call(func: TsGuardExpr, args: TsGuardExpr*): TsGuardExpr =
    Call(func, args.toList)

  def func(args: String*)(body: TsGuardExpr): TsGuardExpr =
    Func(args.toList, body)

  def guard(arg: String, retType: TsType)(body: TsGuardExpr): TsGuardExpr =
    Guard(arg, retType, body)

  def cond(test: TsGuardExpr, trueArm: TsGuardExpr, falseArm: TsGuardExpr): TsGuardExpr =
    Cond(test, trueArm, falseArm)

  def isnull(expr: TsGuardExpr): TsGuardExpr =
    IsNull(expr)

  def not(expr: TsGuardExpr): TsGuardExpr =
    Not(expr)

  def and(lhs: TsGuardExpr, rhss: TsGuardExpr*): TsGuardExpr =
    rhss.foldLeft(lhs)(And(_, _))

  def or(lhs: TsGuardExpr, rhss: TsGuardExpr*): TsGuardExpr =
    rhss.foldLeft(lhs)(Or(_, _))

  def eql(lhs: TsGuardExpr, rhs: TsGuardExpr): TsGuardExpr =
    Eql(lhs, rhs)

  def in(key: String, expr: TsGuardExpr): TsGuardExpr =
    In(key, expr)

  def render(expr: TsGuardExpr): String = {
    val r = renderParens(expr) _

    expr match {
      case Ref(name)                 => name
      case Dot(obj, name)            => s"""${r(obj)}.${name}"""
      case Arr(exprs)                => exprs.map(r).mkString("[", ", ", "]")
      case Index(obj, idx)           => s"""${r(obj)}[${r(idx)}]"""
      case Lit(value)                => value
      case Typeof(expr)              => s"""typeof ${r(expr)}"""
      case Call(func, args)          => s"""${r(func)}(${args.map(render).mkString(", ")})"""
      case Func(args, body)          => s"""(${args.map(_ + ": any").mkString(", ")}) => ${r(body)}"""
      case Guard(arg, retType, body) => s"""(${arg}: any): ${arg} is ${Typescript.renderType(retType)} => ${r(body)}"""
      case Cond(c, t, f)             => s"""${r(c)} ? ${r(t)} : ${r(f)}"""
      case IsNull(expr)              => s"""${r(expr)} == null"""
      case Not(IsNull(expr))         => s"""${r(expr)} != null"""
      case Not(expr)                 => s"""!${r(expr)}"""
      case And(lhs, rhs)             => s"""${r(lhs)} && ${r(rhs)}"""
      case Or(lhs, rhs)              => s"""${r(lhs)} || ${r(rhs)}"""
      case Eql(lhs, rhs)             => s"""${r(lhs)} === ${r(rhs)}"""
      case In(key, expr)             => s"""${r(lit(key))} in ${r(expr)}"""
    }
  }

  private def renderParens(outer: TsGuardExpr)(inner: TsGuardExpr): String =
    if (precedence(outer) > precedence(inner)) {
      s"(${render(inner)})"
    } else {
      render(inner)
    }

  private def precedence(tpe: TsGuardExpr): Int =
    tpe match {
      case _: Ref         => 1000
      case _: Dot         => 1000
      case _: Arr         => 1000
      case _: Index       => 1000
      case _: Lit         => 1000
      case _: Call        => 1000
      case Not(IsNull(_)) => 700
      case _: IsNull      => 700
      case _: Not         => 950
      case _: Typeof      => 900
      case _: Eql         => 700
      case _: In          => 700
      case _: And         => 600
      case _: Or          => 500
      case _: Cond        => 400
      case _: Func        => 300
      case _: Guard       => 300
    }
}
