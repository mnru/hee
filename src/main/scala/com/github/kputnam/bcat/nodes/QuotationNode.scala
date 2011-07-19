package com.github.kputnam.bcat.nodes

import com.github.kputnam.bcat.types._
import com.github.kputnam.bcat.SymbolTable

case class QuotationNode(nodes: List[AbstractNode]) extends AbstractNode {
  // @todo: Need to unify each node with the next, in sequence
  def toType(t: SymbolTable) = None

  override def toString =
    nodes.mkString("[", ", ", "]")
}
