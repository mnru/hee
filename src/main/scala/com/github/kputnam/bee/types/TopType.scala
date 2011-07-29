package com.github.kputnam.bee.types

case object TopType extends MonomorphicType {
  override def toString = "top"

  def unifyWith(t: AbstractType, s: Substitution) =
    throw new UnsupportedOperationException
}
