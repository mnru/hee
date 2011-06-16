package com.github.kputnam.fifth.parser

sealed abstract class Node

case class Word(value: Any) extends Node {
  override def toString = value.toString
}

case class Quotation(nodes: List[Node]) extends Node {
  override def toString =
    nodes.mkString("[", ", ", "]")
}