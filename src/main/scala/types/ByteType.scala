package com.github.kputnam.bee
package types

case object ByteType extends Type with MonomorphicLike {
  override def toString = "byte"
}
