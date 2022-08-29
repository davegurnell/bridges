package bridges.typescript

import bridges.core.{ DeclF, Renderer }
import unindent._

abstract class TsGuardRenderer(
    predName: String => String = id => s"""is${id}""",
    guardName: String => String = id => s"""as${id}"""
) extends Renderer[TsType] {
  import TsType._
  import TsGuardExpr._

  def render(decl: TsDecl): String =
    decl match {
      case DeclF(name, Nil, tpe) =>
        i"""
        export const ${predName(decl.name)} = (v: any): v is ${name} => {
          return ${TsGuardExpr.render(isType(ref("v"), decl.tpe))};
        }
        """

      case DeclF(name, params, tpe) =>
        val tparams = renderParamTypes(params)
        val vparams = renderParamPreds(params)
        i"""
        export const ${predName(decl.name)} = ${tparams}(${vparams}) => (v: any): v is ${name}${tparams} => {
          return ${TsGuardExpr.render(isType(ref("v"), decl.tpe))};
        }
        """
    }

  def renderParamTypes(params: List[String]): String =
    if (params.isEmpty) {
      ""
    } else {
      params.mkString("<", ", ", ">")
    }

  def renderParamPreds(params: List[String]): String =
    params.map(param => s"${predName(param)}: (${param.toLowerCase}: any) => ${param.toLowerCase} is ${param}").mkString(", ")

  import TsGuardExpr._

  def guardFunc(pair: (TsType, Int)): TsGuardExpr = {
    val (tpe, index) = pair
    val arg          = "a" + index
    guard(arg, tpe)(isType(ref(arg), tpe))
  }

  def isType(arg: TsGuardExpr, tpe: TsType): TsGuardExpr =
    tpe match {
      case TsType.Ref(id, Nil) =>
        call(ref(predName(id)), arg)

      case TsType.Ref(id, params) =>
        call(Call(ref(predName(id)), params.zipWithIndex.map(guardFunc)), arg)

      case TsType.Any                  => lit(true)
      case TsType.Unknown              => lit(true)
      case TsType.Str                  => eql(typeof(arg), lit("string"))
      case TsType.Chr                  => eql(typeof(arg), lit("string"))
      case TsType.Intr                 => eql(typeof(arg), lit("number"))
      case TsType.Real                 => eql(typeof(arg), lit("number"))
      case TsType.Bool                 => eql(typeof(arg), lit("boolean"))
      case TsType.StrLit(value)        => eql(arg, lit(value))
      case TsType.ChrLit(value)        => eql(arg, lit(value.toString))
      case TsType.IntrLit(value)       => eql(arg, lit(value))
      case TsType.RealLit(value)       => eql(arg, lit(value))
      case TsType.BoolLit(value)       => eql(arg, lit(value))
      case TsType.Null                 => eql(arg, nullLit)
      case TsType.Arr(tpe)             => isArray(arg, tpe)
      case TsType.Tuple(types)         => isTuple(arg, types)
      case TsType.Func(_, _)           => eql(typeof(arg), lit("function"))
      case TsType.Struct(fields, rest) => isStruct(arg, fields, rest)
      case TsType.Inter(types)         => isAll(arg, types)
      case TsType.Union(types)         => isUnion(arg, types)
    }

  private def isArray(expr: TsGuardExpr, tpe: TsType): TsGuardExpr =
    and(
      call(dot(ref("Array"), "isArray"), expr),
      call(dot(expr, "every"), func("i")(isType(ref("i"), tpe)))
    )

  private def isTuple(expr: TsGuardExpr, types: List[TsType]): TsGuardExpr = {
    val baseChecks = List(
      call(dot(ref("Array"), "isArray"), expr),
      eql(dot(expr, "length"), lit(types.length))
    )

    val itemChecks = types.zipWithIndex.map { case (tpe, idx) => isType(index(expr, idx), tpe) }

    (baseChecks ++ itemChecks).reduceLeft(and(_, _))
  }

  private def isStruct(expr: TsGuardExpr, fields: List[TsField], rest: Option[TsRestField]): TsGuardExpr = {
    val seed = and(eql(typeof(expr), lit("object")), not(isnull(expr)))

    // Are all named fields valid?
    val fieldsExpr: TsGuardExpr = fields
      .map { field =>
        val TsField(name, tpe, optional) = field

        if (optional) {
          or(not(in(name, expr)), isType(dot(expr, name), tpe))
        } else {
          and(in(name, expr), isType(dot(expr, name), tpe))
        }
      }
      .foldLeft(seed)(and(_, _))

    // Is the rest field (if specified) valid?
    val restExpr: Option[TsGuardExpr] =
      rest.map { rest =>
        val fieldNames: List[TsGuardExpr] =
          fields.map(_.name).map(lit)

        def isField: TsGuardExpr =
          call(
            dot(
              arr(fieldNames),
              "includes"
            ),
            ref("k")
          )

        def isRest: TsGuardExpr =
          and(
            isType(ref("k"), rest.keyType),
            isType(index(expr, ref("k")), rest.valueType)
          )

        def isFieldOrRest: TsGuardExpr =
          if (fieldNames.isEmpty) {
            func("k")(isRest)
          } else {
            func("k")(or(isField, isRest))
          }

        call(
          dot(
            call(dot(ref("Object"), "keys"), expr),
            "every"
          ),
          isFieldOrRest
        )
      }

    restExpr.fold(fieldsExpr)(restExpr => and(fieldsExpr, restExpr))
  }

  private def isUnion(expr: TsGuardExpr, types: List[TsType]): TsGuardExpr =
    types.collectAll { case tpe @ DiscriminatedBy(name, rest) => name -> rest } match {
      case Some(pairs) =>
        and(eql(typeof(expr), lit("object")), not(isnull(expr)), in("type", expr), isDiscriminated(expr, pairs))
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
      .reduceLeftOption(or(_, _))
      .getOrElse(lit(false))

  private def isAll(expr: TsGuardExpr, types: List[TsType]): TsGuardExpr =
    types
      .map(isType(expr, _))
      .reduceLeftOption(and(_, _))
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
        case TsType.Struct(fields, _) =>
          fields.collectFirst { case decl @ TsField("type", TsType.StrLit(name), _) =>
            (name, TsType.Struct(fields.filterNot(_ == decl)))
          }

        case _ =>
          None
      }
  }
}
