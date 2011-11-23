package com.github.kputnam.bee
package types

case object BitmapType extends Type with MonomorphicLike {
  override def toString = "bitmap"
}
