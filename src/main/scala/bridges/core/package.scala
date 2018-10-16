package bridges

package object core {
  type Decl     = DeclF[Type]
  type ProdDecl = DeclF[Type.Prod]
}
