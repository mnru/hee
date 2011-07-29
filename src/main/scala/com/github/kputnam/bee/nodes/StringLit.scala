package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._
import com.github.kputnam.bee.SymbolTable

case class StringLit(value: String) extends LiteralNode {
  override def toString = "\"" + value + "\""
  def toType(t: SymbolTable) = Some(StringType)
}
