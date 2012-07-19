package com.github.kputnam.bee.terms

import com.github.kputnam.bee.types._

/**
 * Term that refers to a named value (e.g., "dup"). The name
 * can be dereferenced using the symbol table.
 */
case class NameTerm(name: String) extends Term {
  override def toString = name
}
