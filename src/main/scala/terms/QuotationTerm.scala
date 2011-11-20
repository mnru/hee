package com.github.kputnam.bee.terms

import com.github.kputnam.bee.types._

object QuotationTerm {

  def apply(terms: Term*): QuotationTerm =
    new QuotationTerm(terms.toList)

  def apply(terms: List[Term]): QuotationTerm =
    new QuotationTerm(terms.toList)

  def unapplySeq(q: QuotationTerm): Option[Seq[Term]] =
    Some(q.terms)
}

/**
 * Grouped sequence of terms (e.g., [1 2 3 + *]) whose evaluation is delayed.
 */
class QuotationTerm(val terms: List[Term]) extends Term {

  def head = terms.head
  def tail = if (terms.isEmpty) this else QuotationTerm(terms.tail)
  def isEmpty = terms.isEmpty

  override def toString =
    terms.mkString("QuotationTerm(", ", ", ")")

  override def equals(that: Any) = that match {
    case that: QuotationTerm => this.terms == that.terms
    case _ => false
  }

}
