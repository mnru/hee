package com.github.kputnam.bcat.nodes

import com.github.kputnam.bcat.types._
import com.github.kputnam.bcat.SymbolTable

case class NumericLit(value: BigDecimal) extends LiteralNode {
  override def toString = value.toString
  def toType(s: SymbolTable) = Some(NumericType)
}
