package com.github.kputnam.bee
package static

import types._

/**
 * Kinding contexts     Δ ::= Δ, α:_ | ε
 *   α:σ binds a type variable α to its ...
 *
 * Typing contexts      Γ ::= Γ, x:σ | ε
 *   x:σ binds a term variable x to its type σ
 *
 * Polytypes            σ ::= ∀α.σ | ρ
 *   can have free variables
 *
 * Rho-types            ρ ::= τ | σ → σ
 *
 * Monotypes            τ ::= num | τ → τ | τ ∧ τ | τ × τ | α
 *
 * Type variables       α ::= τa, τb, τc, ...
 *
 * Term variables       x ::= ta, tb, tc, ...
 *
 * Terms                t ::= x         application
 *                          | [t ..]    abstraction
 *
 * Literal values are equivalent to an application of some abstraction
 * that pushes that literal value onto the stack. For instance, we think
 * of the term 1 as an invocation of a procedure that pushes the literal
 * value 1 onto the stack.
 *
 * @TODO: What does the syntax look like for type ascription?
 */
abstract class Context {

  def freeVariables: Set[VariableLike] =
    Set.empty

}
