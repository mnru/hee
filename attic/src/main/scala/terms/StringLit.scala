package com.github.kputnam.bee.terms

import com.github.kputnam.bee.types._

case class StringLit(value: String) extends LiteralTerm {
  override def toString = "StringLit(\"" + value + "\")"
}
