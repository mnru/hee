package com.github.kputnam.bee
package types

case object TopType extends Type with MonomorphicLike {
  override def toString = "any"
}
