package com.github.kputnam.fifth

import types._

object SymbolTable {
  def default =
    SymbolTable(Map.empty, None).
      // T a pop :: T
      addBinding("pop",
        WordType(StackType(TypeVariable(0),
                           RestVariable(1)),
                 StackType(RestVariable(1)))).

      // T a dup :: T a a
      addBinding("dup",
        WordType(StackType(TypeVariable(0),
                           RestVariable(1)),
                 StackType(TypeVariable(0),
                           TypeVariable(0),
                           RestVariable(1)))).

      // T id :: T
      addBinding("id",
        WordType(StackType(new RestVariable(0)),
                 StackType(new RestVariable(0)))).

      // T a b swap :: T b a
      addBinding("swap",
        WordType(StackType(TypeVariable(0),
                           TypeVariable(1),
                           RestVariable(2)),
                 StackType(TypeVariable(1),
                           TypeVariable(0),
                           RestVariable(2)))).

      // S (A -> B) (B -> C) compose :: S (A -> B)
      addBinding("compose",
        WordType(StackType(WordType(StackType(RestVariable(1)),  // B -> C
                                    StackType(RestVariable(0))),
                           WordType(StackType(RestVariable(2)),  // A -> B
                                    StackType(RestVariable(1))),
                           RestVariable(3)),
                 StackType(WordType(StackType(RestVariable(2)),  // A -> C
                                    StackType(RestVariable(0))),
                           RestVariable(3)))).

      // A (A -> B) apply :: B
      addBinding("apply",
        WordType(StackType(WordType(StackType(RestVariable(0)),   // A -> B
                                    StackType(RestVariable(1))),
                           RestVariable(0)),                      // A
                 StackType(RestVariable(1)))).                    // B

      // A a quote :: A (B -> a)
      addBinding("quote",
        WordType(StackType(TypeVariable(0), RestVariable(1)),
                 StackType(WordType(StackType(RestVariable(2)),
                                    StackType(TypeVariable(0),
                                              RestVariable(2))),
                           RestVariable(1))))
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
    get(name).orElse(Some(default)) match { case Some(t) => t }
}