package esmeta.compiler

import esmeta.lang.Type
import esmeta.ty.*
import esmeta.ty.util.{Walker => TyWalker}

object TyCompiler extends TyWalker {
  override def walk(ty: UnknownTy): UnknownTy =
    UnknownTy(ty.msg.map(Type.normalizeName))
}
