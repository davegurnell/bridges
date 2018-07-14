package bridges.core

import unindent._

trait JsonEncoder[A] {
  def encoder(decl: Declaration): String
  def encoder(decls: List[Declaration]): String =
    decls.map(encoder).mkString("\n\n")
}
