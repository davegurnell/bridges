package bridges.core

/** A named declaration. Either top-level or a field in a sum, product, or struct.
  *
  * We define type aliases for DeclF in
  * the package objects for bridges.core, bridges.flow, and bridges.typescript.
  */
final case class DeclF[+A](name: String, params: List[String], tpe: A) {
  def map[B](func: A => B): DeclF[B] =
    copy(tpe = func(tpe))
}

object DeclF {
  def apply[A](name: String, tpe: A): DeclF[A] =
    DeclF(name, Nil, tpe)

  implicit def rename[A](implicit rename: Rename[A]): Rename[DeclF[A]] =
    Rename.pure[DeclF[A]] { (decl, from, to) =>
      DeclF(
        if (decl.name == from) to else decl.name,
        decl.params,
        if (decl.params.contains(from)) decl.tpe else rename(decl.tpe, from, to)
      )
    }
}
