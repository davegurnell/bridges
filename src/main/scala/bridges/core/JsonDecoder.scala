package bridges.core

import unindent._

trait JsonDecoder[A] {
  def decoder(decl: Declaration): String
  def decoder(decls: List[Declaration]): String =
    decls.map(decoder).mkString("\n\n")
}
