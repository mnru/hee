package com.github.kputnam.bcat.nodes

import com.github.kputnam.bcat.types._
import com.github.kputnam.bcat.SymbolTable

case class BooleanLit(value: Boolean) extends LiteralNode {
  override def toString = value.toString
  def toType(s: SymbolTable) = Some(BooleanType)
}
