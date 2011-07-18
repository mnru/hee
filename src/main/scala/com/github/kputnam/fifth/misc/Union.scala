package com.github.kputnam.fifth.misc

object union {
  type ¬[A] = A => Nothing
  type ¬¬[A] = ¬[¬[A]]

  type N[T, U] = ¬[¬[T] with ¬[U]]
  type Union[T, U] = { type Value[X] = ¬¬[X] <:< N[T, U] }
}