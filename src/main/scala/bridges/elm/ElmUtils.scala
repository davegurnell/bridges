package bridges.elm

import bridges.core.Decl
import bridges.core.Type.Ref

trait ElmUtils {

  // helper method intended to be used to unify how we work with hand made generics and custom type replacements in Elm
  def mergeGenericsAndTypes(decl: Decl, customTypeReplacements: Map[Ref, TypeReplacement]): (Map[Ref, TypeReplacement], List[String]) = {
    val genericsAsElmReplacement = decl.params.map(k => Ref(k) -> TypeReplacement(k.toLowerCase)).toMap
    val newTypeReplacements      = customTypeReplacements ++ genericsAsElmReplacement
    val genericsDefinition       = genericsAsElmReplacement.valuesIterator.map(_.newType).toList

    (newTypeReplacements, genericsDefinition)
  }

}
