package com.github.kputnam.bcat.nodes

import com.github.kputnam.bcat.types._
import com.github.kputnam.bcat.SymbolTable

case class WordNode(name: String) extends AbstractNode {
  override def toString = name
  def toType(t: SymbolTable) = t.get(name)
}
