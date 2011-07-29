package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._
import com.github.kputnam.bee.SymbolTable

case class NumericLit(value: BigDecimal) extends LiteralNode {
  override def toString = value.toString
  def toType(s: SymbolTable) = Some(NumericType)
}
