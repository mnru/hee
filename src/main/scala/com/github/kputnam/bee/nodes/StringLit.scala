package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._

case class StringLit(value: String) extends LiteralNode {
  override def toString = "\"" + value + "\""
}
