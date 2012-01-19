/**
 * Deep-skolemization [Practical Type Inference for Arbitrary-Rank Types]
 *
 * Universal quantifiers that appear on the right of an
 * outer-level → can be shifted upwards near the root.
 * 
 *   (∀a.(∀b.(a → (b → b)))) = (∀a.(a → (∀b.(b → b))))
 *
 * Use alpha-conversion to avoid variable capture of a
 *
 *   (∀a.(∀b.(a → (b → b)))) = (∀a.(a → (∀a.(a → a))))
 *
 * The rule only applies to universal quantifiers on the
 * left, not the right, of an outer-level →.
 *
 *   ((∀a.(a → a)) → (∀a.(a → a))) = (∀b.(∀a.(a → a)) → b → b)
 */
