package bridges.core

trait Rename[A]:
  def apply(value: A, from: String, to: String): A

object Rename:
  def apply[A](using rename: Rename[A]): Rename[A] =
    rename

  def pure[A](func: (A, String, String) => A): Rename[A] =
    new Rename[A]:
      override def apply(value: A, from: String, to: String): A =
        func(value, from, to)

  given pairRename[A](using aRename: Rename[A]): Rename[(String, A)] =
    Rename.pure { (pair, from, to) =>
      pair match {
        case (name, a) => (if name == from then to else name, aRename(a, from, to))
      }
    }

trait RenamableSyntax:
  extension [A](value: A)
    def rename(from: String, to: String)(using rename: Rename[A]): A =
      rename(value, from, to)
