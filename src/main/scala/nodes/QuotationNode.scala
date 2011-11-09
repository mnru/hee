package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._

object QuotationNode {
  def apply(nodes: AbstractNode*): QuotationNode =
    new QuotationNode(nodes.toList)

  def apply(nodes: List[AbstractNode]): QuotationNode =
    new QuotationNode(nodes.toList)

  def unapplySeq(q: QuotationNode): Option[Seq[AbstractNode]] =
    Some(q.nodes)
}

class QuotationNode(val nodes: List[AbstractNode]) extends AbstractNode {
  def head = nodes.head
  def tail = if (nodes.isEmpty) this else QuotationNode(nodes.tail)
  def isEmpty = nodes.isEmpty

  override def toString =
    nodes.mkString("[", ", ", "]")

  override def equals(that: Any) = that match {
    case that: QuotationNode => this.nodes == that.nodes
    case _ => false
  }
}
