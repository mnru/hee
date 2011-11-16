package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._

/**
 * Syntax node that refers to a named value (e.g., "dup"). The name
 * can be dereferenced using the symbol table.
 */
case class NameNode(name: String) extends AbstractNode {
  override def toString = name
}
