package com.github.kputnam.bee
package types

case object CharacterType extends Type with MonomorphicLike {
  override def toString = "char"
}
