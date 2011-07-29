package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._
import com.github.kputnam.bee.SymbolTable

case class WordNode(name: String) extends AbstractNode {
  def toType(t: SymbolTable) = t.get(name)

  override def toString = name
}
