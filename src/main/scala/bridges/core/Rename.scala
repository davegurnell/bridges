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
}

trait RenamableSyntax {
  implicit class RenamableOps[A](value: A) {
    def rename(from: String, to: String)(implicit rename: Rename[A]): A =
      rename(value, from, to)
  }
}
