package bridges.core

import shapeless.{ Lazy, Typeable }

/** A named declaration. Either top-level or a field in a sum, product, or struct.
  *
  * We define type aliases for DeclF in
  * the package objects for bridges.core, bridges.flow, and bridges.typescript.
  */
final case class DeclF[+A](name: String, tpe: A) {
  def map[B](func: A => B): DeclF[B] =
    copy(tpe = func(tpe))
}

object DeclF {
  implicit def rename[A](implicit rename: Rename[A]): Rename[DeclF[A]] =
    Rename.pure[DeclF[A]] { (decl, from, to) =>
      DeclF(
        if (decl.name == from) to else decl.name,
        rename(decl.tpe, from, to)
      )
    }
}
