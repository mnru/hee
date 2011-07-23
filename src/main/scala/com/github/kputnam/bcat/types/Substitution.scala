package com.github.kputnam.bcat.types

object Substitution {
  def empty = Substitution(Map.empty[Variable, AbstractType])
  def apply(bindings: Pair[Variable, AbstractType]*): Substitution =
    Substitution(Map(bindings:_*))
}

case class Substitution(bindings: Map[Variable, AbstractType]) {
  override def toString =
    "Substitution(" + bindings.foldLeft(new StringBuilder) {(s, binding) =>
      if (!s.isEmpty)
        s.append(", ")
      s.append(binding._1)
      s.append("/")
      s.append(binding._2)} + ")"

  def variables: Set[Variable] = bindings.keys.toSet

  def getOrElse(k: Variable, default: AbstractType) =
    bindings.getOrElse(k, default)

  def addBinding(k: Variable, v: AbstractType): Substitution = {
    if (k == v)
      return this

    // Substitute v for all occurrences of k in existing bindings
    val singleton = Substitution(Map(k -> v))
    Substitution(bindings.mapValues(v => v.substitute(singleton)) + (k -> v))
  }
}
