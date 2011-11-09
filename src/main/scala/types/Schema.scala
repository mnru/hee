package com.github.kputnam.bee.types

/**
 * σ := ∀x.σ
 *    | τ
 */
case class Schema(quantified: Set[VariableLike], expression: Type)
