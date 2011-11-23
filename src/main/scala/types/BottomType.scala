package com.github.kputnam.bee
package types

case object BottomType extends Type with MonomorphicLike {
  override def toString = "none"
}
