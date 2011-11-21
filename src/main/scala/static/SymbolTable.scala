package com.github.kputnam.bee
package static

import types.Type

/**
 * Note that because bee doesn't support mutable bindings (variables), the
 * symbol table doesn't need to track them. We still need to track definitions
 * of names, which are equivalent to constant values, and types.
 *
 * Future attributes
 * - value
 * - source location
 * - compiled address
 *
 * Research
 * - unresolved overloading
 * - instance-of (reflexive, transitive)
 * - anonymous recursive types (mu)
 */
case class Entry(name: String, τ: Type)

abstract class SymbolTable {
  def bindings: Set[Entry]
  def addBinding(x: String, τ: Type): SymbolTable
  def searchBindings(x: String): Option[Entry]
}

case object Root extends SymbolTable {
  def addBinding(name: String, τ: Type) =
    throw new UnsupportedOperationException

  def bindings = Set.empty
  def searchBindings(x: String): Option[Entry] = None
}

case class Child(val parent: SymbolTable, bs: Map[String, Entry]) extends SymbolTable {
  /** Bind a new type to the given name */
  def addBinding(x: String, τ: Type): SymbolTable =
    new Child(parent, bs + (x -> Entry(x, τ)))

  /** Bindings in this node can shadow parent bindings */
  def bindings =
    parent.bindings ++ bs.values.toSet

  /** Filter bindings by name */
  def searchBindings(x: String): Option[Entry] =
    bs.get(x) //getOrElse(x, parent.searchBindings(x))
}

object SymbolTable {
  import com.github.kputnam.bee.types.{NonEmpty => _, Empty => _, _}
  import com.github.kputnam.bee.types.WordType._

  def root =
    Root

  def fromParent(parent: SymbolTable) =
    new Child(parent, Map.empty)

  def default =
    fromParent(root).
      /** Combinators
       ***********************************************************************/
      addBinding("id", // T id :: T
        WordType(Tail(0),
                 Tail(0))).

      addBinding("pop", // T a pop :: T
        WordType(Tail(0) :+ Variable(1),
                 Tail(0))).

      addBinding("dup", // T a dup :: T a a
        WordType(Tail(0) :+ Variable(1),
                 Tail(0) :+ Variable(1) :+ Variable(1))).

      addBinding("swap", // T a b swap :: T b a
        WordType(Tail(0) :+ Variable(1) :+ Variable(2),
                 Tail(0) :+ Variable(2) :+ Variable(1))).

      addBinding("apply", // A (A -> B) apply :: B
        WordType(Tail(0) :+ WordType(Tail(0), Tail(1)),
                 Tail(1))).

      addBinding("dip", // T (T -> S) a dip :: S a
        WordType(Tail(0) :+ WordType(Tail(0),
                                     Tail(2) :+ Variable(1)),
                 Tail(2) :+ Variable(1))).

      addBinding("quote", // A a quote :: A (B -> B a)
        WordType(Tail(0) :+ Variable(1),
                 Tail(0) :+ WordType(Tail(2),
                                     Tail(2) :+ Variable(1)))).

      addBinding("compose", // S (A -> B) (B -> C) compose :: S (A -> C)
        WordType(Tail(0) :+ WordType(Tail(1), Tail(2))
                         :+ WordType(Tail(2), Tail(3)),
                 Tail(0) :+ WordType(Tail(1), Tail(3)))).

      /** Control flow
       ***********************************************************************/
      addBinding("if", // S boolean a a if :: S a
        WordType(Tail(0) :+ BooleanType :+ Variable(1) :+ Variable(1),
                 Tail(0) :+ Variable(1))).

      addBinding("halt", // A :: ∅
        WordType(Tail(0),
                 StackType.empty))
}
