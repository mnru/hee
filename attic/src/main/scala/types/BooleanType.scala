package com.github.kputnam.bee
package types

case object BooleanType extends Type with MonomorphicLike {
  override def toString = "bool"
}
