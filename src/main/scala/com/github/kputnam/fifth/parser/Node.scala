package com.github.kputnam.fifth.parser

import com.github.kputnam.fifth.types.{NumericType, BooleanType, StringType}

sealed abstract class Node

case class Quotation(nodes: List[Node]) extends Node {
  override def toString =
    nodes.mkString("[", " ", "]")
}

case class Word(name: String) extends Node {
  override def toString = name
}

sealed abstract class Literal extends Node

case class StringLit(value: String) extends Literal {
  def toType = StringType
  override def toString = "\"" + value + "\""
}

case class BooleanLit(value: Boolean) extends Literal {
  def toType = BooleanType
  override def toString = value.toString
}

case class NumericLit(value: BigDecimal) extends Literal {
  def toType = NumericType
  override def toString = value.toString
}
