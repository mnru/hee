package com.github.kputnam.bee
package types

case object StringType extends Type with MonomorphicLike {
  override def toString = "str"
}
