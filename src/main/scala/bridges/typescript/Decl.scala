package bridges.typescript

final case class Decl(name: String, params: List[String], tpe: TsType)

object Decl:
  def apply(name: String, tpe: TsType): Decl =
    Decl(name, Nil, tpe)
