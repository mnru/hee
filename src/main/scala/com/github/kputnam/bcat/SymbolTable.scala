package com.github.kputnam.bcat

import com.github.kputnam.bcat.types._

object SymbolTable {
  def default =
    SymbolTable(Map.empty, None).

      // Combinators
      addBinding("id", // T id :: T
        WordType(Remainder(0),
                 Remainder(0))).
      addBinding("pop", // T a pop :: T
        WordType(Remainder(0) :+ TypeVariable(1),
                 Remainder(0))).
      addBinding("dup", // T a dup :: T a a
        WordType(Remainder(0) :+ TypeVariable(1),
                 Remainder(0) :+ TypeVariable(1) :+ TypeVariable(1))).
      addBinding("swap", // T a b swap :: T b a
        WordType(Remainder(0) :+ TypeVariable(1) :+ TypeVariable(2),
                 Remainder(0) :+ TypeVariable(2) :+ TypeVariable(1))).
      addBinding("compose", // S (A -> B) (B -> C) compose :: S (A -> C)
        WordType(Remainder(0) :+ WordType(Remainder(1),
                                          Remainder(2))
                              :+ WordType(Remainder(2),
                                          Remainder(3)),
                 Remainder(0) :+ WordType(Remainder(1),
                                          Remainder(3)))).
      addBinding("apply", // A (A -> B) apply :: B
        WordType(Remainder(0) :+ WordType(Remainder(0),
                                          Remainder(1)),
                 Remainder(1))).
      addBinding("dip", // T a (T -> S) dip :: S a
        WordType(Remainder(0) :+ TypeVariable(1)
                              :+ WordType(Remainder(0),
                                          Remainder(2)),
                 Remainder(2) :+ TypeVariable(1))).
      addBinding("quote", // A a quote :: A (B -> B a)
        WordType(Remainder(0) :+ TypeVariable(1),
                 Remainder(0) :+ WordType(Remainder(2),
                                          Remainder(2) :+ TypeVariable(1)))).

      addBinding("if", // A boolean a a if :: A a
        WordType(Remainder(0) :+ BooleanType :+ TypeVariable(1) :+ TypeVariable(1),
                 Remainder(0) :+ TypeVariable(1))).

      // Numeric operators
      addBinding("+",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("-",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("*",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("/",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("%",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("/%",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType :+ NumericType)).
      addBinding("**",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).

      // Numeric relational operators
      addBinding("=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding("!=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding("<",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding("<=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding(">=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).
      addBinding(">",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ BooleanType)).

      // Bitwise operators
      addBinding("&",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("^",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("~",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("|",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).

      // Logical operators
      addBinding("~",
        WordType(Remainder(0) :+ BooleanType,
                 Remainder(0) :+ BooleanType)).
      addBinding("&",
        WordType(Remainder(0) :+ BooleanType :+ BooleanType,
                 Remainder(0) :+ BooleanType)).
      addBinding("^",
        WordType(Remainder(0) :+ BooleanType :+ BooleanType,
                 Remainder(0) :+ BooleanType)).
      addBinding("|",
        WordType(Remainder(0) :+ BooleanType :+ BooleanType,
                 Remainder(0) :+ BooleanType)).

      // String operators
      addBinding("length",
        WordType(Remainder(0) :+ StringType,
                 Remainder(0) :+ NumericType)).
      addBinding("empty?",
        WordType(Remainder(0) :+ StringType,
                 Remainder(0) :+ BooleanType)).
      addBinding("+",
        WordType(Remainder(0) :+ StringType :+ StringType,
                 Remainder(0) :+ StringType)).
      addBinding("take",
        WordType(Remainder(0) :+ StringType :+ NumericType,
                 Remainder(0) :+ StringType)).
      addBinding("drop",
        WordType(Remainder(0) :+ StringType :+ NumericType,
                 Remainder(0) :+ StringType)).
      addBinding("includes?",
        WordType(Remainder(0) :+ StringType :+ StringType,
                 Remainder(0) :+ BooleanType))
}

case class SymbolTable(ts: Map[String, AbstractType], parent: Option[SymbolTable]) {
  def isBound(name: String) =
    ts.contains(name)

  def addScope(name: String) =
    SymbolTable(Map.empty, Some(this))

  def addBinding(name: String, t: AbstractType) =
    SymbolTable(ts + (name -> t), parent)

  def get(name: String): Option[AbstractType] =
    ts.get(name).orElse(parent.flatMap(_.get(name)))

  def getOrElse(name: String, default: => AbstractType) =
    ts.getOrElse(name, default)
}
