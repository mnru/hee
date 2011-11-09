package com.github.kputnam.bee.nodes

import com.github.kputnam.bee.types._

case class WordNode(name: String) extends AbstractNode {
  override def toString = name
}
