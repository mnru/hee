package com.github.kputnam.bcat.nodes

import com.github.kputnam.bcat.types._
import com.github.kputnam.bcat.SymbolTable

object QuotationNode {
  def apply(nodes: AbstractNode*): QuotationNode =
    new QuotationNode(nodes.toList)

  def apply(nodes: List[AbstractNode]): QuotationNode =
    new QuotationNode(nodes.toList)

  def unapplySeq(q: QuotationNode): Option[Seq[AbstractNode]] =
    Some(q.nodes)
}

class QuotationNode(val nodes: List[AbstractNode]) extends AbstractNode {
  // @todo: prevent nodes from being an empty list

  def toType(s: SymbolTable) = None

  /*def toType(t: SymbolTable) = nodes match {
    case head :: Nil  => StackType(head.
    case head :: tail =>
  }*/

  def head = nodes.head
  def tail = if (nodes.lengthCompare(1) > 1) Some(QuotationNode(nodes.tail)) else None

  override def toString =
    nodes.mkString("[", ", ", "]")

  override def equals(that: Any) = that match {
    case that: QuotationNode => this.nodes == that.nodes
    case _ => false
  }
}
