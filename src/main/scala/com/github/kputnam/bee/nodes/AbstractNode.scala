package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types.{AbstractType, WordType}
import com.github.kputnam.bee.SymbolTable

abstract class AbstractNode {
  def toType(s: SymbolTable): Option[AbstractType]
  def wordType(s: SymbolTable): Option[WordType] = toType(s).map(_.asWord)
}
