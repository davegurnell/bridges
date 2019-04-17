package bridges.core

trait Rename[A] {
  def apply(value: A, from: String, to: String): A
}

object Rename {
  def apply[A](implicit rename: Rename[A]): Rename[A] =
    rename

  def pure[A](func: (A, String, String) => A): Rename[A] =
    new Rename[A] {
      override def apply(value: A, from: String, to: String): A =
        func(value, from, to)
    }

  implicit def pairRename[A](implicit aRename: Rename[A]): Rename[(String, A)] =
    Rename.pure { (pair, from, to) =>
      pair match {
        case (name, a) => (if (name == from) to else name, aRename(a, from, to))
      }
    }
}

trait RenamableSyntax {
  implicit class RenamableOps[A](value: A) {
    def rename(from: String, to: String)(implicit rename: Rename[A]): A =
      rename(value, from, to)
  }
}
