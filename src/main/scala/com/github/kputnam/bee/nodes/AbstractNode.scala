package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types.AbstractType
import com.github.kputnam.bee.SymbolTable

abstract class AbstractNode {
  def toType(s: SymbolTable): Option[AbstractType]
}
