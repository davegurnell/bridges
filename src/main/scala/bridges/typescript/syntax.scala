package bridges.typescript

object syntax:
  inline def decl[A](using enc: AdtEncoder[A]): Decl =
    lazy val name = TypeName.of[A]
    Decl(name, enc.encode)

  def decl(name: String, params: String*)(tpe: TsType): Decl =
    Decl(name, params.toList, tpe)

  extension (name: String)
    def -->(tpe: TsType): TsField =
      TsField(name, tpe)

    def -?>(tpe: TsType): TsField =
      TsField(name, tpe, true)
