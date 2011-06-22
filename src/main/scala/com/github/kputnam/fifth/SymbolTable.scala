package com.github.kputnam.fifth

import types._

object SymbolTable {
  def default =
    SymbolTable(Map.empty, Map.empty, None).
      // a pop ::
      addBinding("pop",
        WordType(StackType(UnknownType(0)),
                 StackType())).

      // a dup :: a a
      addBinding("dup",
        WordType(StackType(UnknownType(0)),
                 StackType(UnknownType(0), UnknownType(0)))).

      // a id :: a
      addBinding("id",
        WordType(StackType(UnknownType(0)),
                 StackType(UnknownType(0)))).

      // b a swap :: a b
      addBinding("swap",
        WordType(StackType(UnknownType(0), UnknownType(1)),
                 StackType(UnknownType(1), UnknownType(0)))).

      // (S -> T) (T -> U) compose :: (S -> U)
      addBinding("compose",
        WordType(StackType(WordType(UnknownType(0), UnknownType(1)),
                           WordType(UnknownType(1), UnknownType(2))),
                 StackType(WordType(UnknownType(0), UnknownType(2))))).

      // S (S -> T) apply :: T
      addBinding("apply",
        WordType(StackType(UnknownType(0), WordType(UnknownType(0), UnknownType(1))),
                 StackType(UnknownType(1)))).

      // a quote :: (S -> a)
      addBinding("quote",
        WordType(StackType(UnknownType(0)),
                 StackType(
                   WordType(UnknownType(1),
                            StackType(UnknownType(0))))))
}

case class SymbolTable(ts: Map[String, Type], parent: Option[SymbolTable]) {
  def isBound(name: String) =
    ts.contains(name)

  def addScope(name: String) =
    SymbolTable(Map.empty, Some(this))

  def addBinding(name: String, t: Type) =
    SymbolTable(ts + (name -> t), parent)

  def get(name: String): Option[Type] =
    ts.get(name).orElse(parent.flatMap(_.get(name)))

  def getOrElse(name: String, default: => Type) =
    get(name).orElse(Some(default))
}