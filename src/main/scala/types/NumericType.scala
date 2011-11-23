package com.github.kputnam.bee
package types

case object NumericType extends Type with MonomorphicLike {
  override def toString = "num"
}
