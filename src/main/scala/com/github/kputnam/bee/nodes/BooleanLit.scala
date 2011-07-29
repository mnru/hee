package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._
import com.github.kputnam.bee.SymbolTable

case class BooleanLit(value: Boolean) extends LiteralNode {
  override def toString = value.toString
  def toType(s: SymbolTable) = Some(BooleanType)
}
