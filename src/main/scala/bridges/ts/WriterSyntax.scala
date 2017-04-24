package bridges.ts

import java.io.{File, FileWriter, PrintWriter, Writer}

trait WriterSyntax {
  self: RendererSyntax =>

  import Type._

  implicit class BindingListWriteOps(bindings: List[Binding]) {
    def writeTo(file: File): Unit = {
      val out = new PrintWriter(new FileWriter(file))
      try {
        out.println(bindings.render)
      } finally {
        out.close
      }
    }

    def writeTo(writer: Writer): Unit = {
      val out = new PrintWriter(writer)
      out.println(bindings.render)
    }
  }
}
