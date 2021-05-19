package bridges.core

type Decl = DeclF[Type]

case class DeclF[+A](name: String, params: List[String], tpe: A):
  def map[B](func: A => B): DeclF[B] =
    copy(tpe = func(tpe))

object DeclF:
  def apply[A](name: String, tpe: A): DeclF[A] =
    DeclF(name, Nil, tpe)

  implicit def rename[A](implicit rename: Rename[A]): Rename[DeclF[A]] =
    Rename.pure[DeclF[A]] { (decl, from, to) =>
      DeclF(
        if decl.name == from then to else decl.name,
        decl.params,
        if decl.params.contains(from) then decl.tpe else rename(decl.tpe, from, to)
      )
    }
