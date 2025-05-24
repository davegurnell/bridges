package bridges

/** A named declaration. Either top-level or a field in a sum, product, or struct. */
final case class TsDecl(name: String, params: List[String], tpe: TsType) {
  def map(func: TsType => TsType): TsDecl =
    copy(tpe = func(tpe))
}

object TsDecl {
  def apply(name: String, tpe: TsType): TsDecl =
    TsDecl(name, Nil, tpe)

  implicit val rename: Rename[TsDecl] =
    Rename.instance { (decl, from, to) =>
      val TsDecl(name, params, tpe) = decl
      TsDecl(
        if (name == from) to else name,
        params,
        if (params.contains(from)) tpe else Rename.rename(tpe, from, to)
      )
    }
}
