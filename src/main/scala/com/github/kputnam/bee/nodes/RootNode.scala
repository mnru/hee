package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._
import com.github.kputnam.bee.SymbolTable

object RootNode {
  def apply(nodes: AbstractNode*): RootNode =
    new RootNode(nodes.toList)

  def apply(nodes: List[AbstractNode]): RootNode =
    new RootNode(nodes.toList)

  def unapplySeq(q: RootNode): Option[Seq[AbstractNode]] =
    Some(q.nodes)
}

class RootNode(val nodes: List[AbstractNode]) extends AbstractNode {
  def toType(s: SymbolTable) =
    if (isEmpty)
      // empty quotation is like id :: S -> S
      Some(WordType(Remainder(0), Remainder(0)))
    else
      (nodes.head.wordType(s) /: nodes.tail)((t, n) =>
        t.flatMap(t => n.wordType(s).flatMap(n => t.chainInto(n.rename(t.freeVariables)))))
  
  def head = nodes.head
  def tail = if (nodes.isEmpty) this else QuotationNode(nodes.tail)
  def isEmpty = nodes.isEmpty

  override def toString =
    nodes.mkString("[", ", ", "]")

  override def equals(that: Any) = that match {
    case that: RootNode => this.nodes == that.nodes
    case _ => false
  }
}
