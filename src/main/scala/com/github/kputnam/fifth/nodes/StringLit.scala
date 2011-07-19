package com.github.kputnam.bcat.nodes

import com.github.kputnam.bcat.types._
import com.github.kputnam.bcat.SymbolTable

case class StringLit(value: String) extends LiteralNode {
  override def toString = "\"" + value + "\""
  def toType(t: SymbolTable) = Some(StringType)
}
