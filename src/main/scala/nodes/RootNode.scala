package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._

object RootNode {
  def apply(nodes: AbstractNode*): RootNode =
    new RootNode(nodes.toList)

  def apply(nodes: List[AbstractNode]): RootNode =
    new RootNode(nodes.toList)

  def unapplySeq(q: RootNode): Option[Seq[AbstractNode]] =
    Some(q.nodes)
}

class RootNode(val nodes: List[AbstractNode]) extends AbstractNode {
  def head = nodes.head
  def tail = if (nodes.isEmpty) this else QuotationNode(nodes.tail)
  def isEmpty = nodes.isEmpty

  override def toString =
    nodes.mkString("RootNode(", ", ", ")")

  override def equals(that: Any) = that match {
    case that: RootNode => this.nodes == that.nodes
    case _ => false
  }
}
