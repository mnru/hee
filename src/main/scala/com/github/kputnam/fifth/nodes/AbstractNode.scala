package com.github.kputnam.bcat.nodes

import com.github.kputnam.bcat.types.AbstractType
import com.github.kputnam.bcat.SymbolTable

abstract class AbstractNode {
  def toType(s: SymbolTable): Option[AbstractType]
}
