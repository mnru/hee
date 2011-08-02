package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._
import com.github.kputnam.bee.SymbolTable

object QuotationNode {
  def apply(nodes: AbstractNode*): QuotationNode =
    new QuotationNode(nodes.toList)

  def apply(nodes: List[AbstractNode]): QuotationNode =
    new QuotationNode(nodes.toList)

  def unapplySeq(q: QuotationNode): Option[Seq[AbstractNode]] =
    Some(q.nodes)
}

class QuotationNode(val nodes: List[AbstractNode]) extends AbstractNode {
  def toType(s: SymbolTable) =
    nodes.tail.foldLeft(nodes.head.toType(s).map(_.asWord)) ((t, n) =>
      t.flatMap(t =>
        n.toType(s).flatMap(u =>
          t.chainInto(u.asWord.rename(t.freeVariables), Substitution.empty)).map(_._1))).map(_.quote)
  
  def head = nodes.head
  def tail = if (isLast) None else Some(QuotationNode(nodes.tail))
  def isLast = nodes.lengthCompare(1) <= 0

  override def toString =
    nodes.mkString("[", ", ", "]")

  override def equals(that: Any) = that match {
    case that: QuotationNode => this.nodes == that.nodes
    case _ => false
  }
}
