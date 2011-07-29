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
    nodes.tail.foldLeft(nodes.head.toType(s).map(_.asWord)) ((t, n) =>
      t.flatMap(t =>
        n.toType(s).flatMap(u =>
          t.chainInto(u.asWord.rename(t.variables), Substitution.empty)).map(_._1)))
  
  def head = nodes.head
  def tail = if (isLast) None else Some(RootNode(nodes.tail))
  def isLast = nodes.lengthCompare(1) <= 0

  override def toString =
    nodes.mkString("[", ", ", "]")

  override def equals(that: Any) = that match {
    case that: RootNode => this.nodes == that.nodes
    case _ => false
  }
}
