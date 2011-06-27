package com.github.kputnam.fifth.types

object Substitution {
  def empty = Substitution(Map.empty)
}

case class Substitution(bs: Map[Variable, Type]) {
  override def toString =
    "Substitution(" + bs.foldLeft(new StringBuilder) {(s, b) =>
      if (!s.isEmpty)
        s.append(", ")
      s.append(b._1)
      s.append(": ")
      s.append(b._2)} + ")"

  def getOrElse(k: Variable, default: Type) =
    bs.getOrElse(k, default)

  def addBinding(k: Variable, v: Type): Option[Substitution] = {
    val s = Substitution(Map(k -> v))
    bs.foldLeft(Some(Map.empty): Option[Map[Variable, Type]]) ((bs, b) =>
      bs.flatMap(bs => b._2.substitute(s).map(t => bs + (b._1 -> t)))
    ).map(bs => Substitution(bs + (k -> v)))
  }
}