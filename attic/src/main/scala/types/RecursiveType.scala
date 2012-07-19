package com.github.kputnam.bee
package types

import static._

/**
 * Recursive type (defined in terms of itself), required to
 * type recursive data structures
 */
case class RecursiveType(α: Variable, τ: Type) extends Type {

  override
  def toString =
    "(μ" + α + "." + τ + ")"

  def freeVariables =
    τ.freeVariables - α

  def substitute(s: Substitution) =
    RecursiveType(α, τ.substitute(s \ α))

  override def skolemize =
    RecursiveType(α, τ.skolemize)

}
