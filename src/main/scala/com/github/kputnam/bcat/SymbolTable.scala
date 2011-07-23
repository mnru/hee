package com.github.kputnam.bcat

import com.github.kputnam.bcat.types._

/**
 * def word(s: Name) = SymbolTable.default.getOrElse(word, null).asInstanceOf[WordType]
 *
 * val s = StackType(StringType, IntegerType, BooleanType)
 *   s.top // BooleanType
 *
 * word("pop").input.unifyWith(s, Substitution.empty).
 *   flatMap(word("pop").output.substitute(_))
 *   // Some([StringType, IntegerType])
 *
 * word("dup").input.unifyWith(s, Substitution.empty).
 *   flatMap(word("dup").output.substitute(_))
 *   // Some([StringType, IntegerType, BooleanType, BooleanType])
 *
 * word("id").input.unifyWith(s, Substitution.empty).
 *   flatMap(word("id").output.substitute(_))
 *   // Some([StringType, IntegerType, BooleanType])
 *
 * word("swap").input.unifyWith(s, Substitution.empty).
 *   flatMap(word("swap").output.substitute(_))
 *   // Some([StringType, BooleanType, IntegerType])
 *
 * word("quote").input.unifyWith(s, Substitution.empty).
 *   flatMap(word("quote").output.substitute(_))
 *   // Some([StringType, IntegerType, (C -> [C, BooleanType])])
 */
object SymbolTable {
  def default =
    SymbolTable(Map.empty, None).
      // T a pop :: T
      addBinding("pop",
        WordType(Remainder(0) :+ TypeVariable(1),
                 Remainder(0))).

      // T a dup :: T a a
      addBinding("dup",
        WordType(Remainder(0) :+ TypeVariable(1),
                 Remainder(0) :+ TypeVariable(1) :+ TypeVariable(1))).

      // T id :: T
      addBinding("id",
        WordType(Remainder(0),
                 Remainder(0))).

      // T a b swap :: T b a
      addBinding("swap",
        WordType(Remainder(0) :+ TypeVariable(1) :+ TypeVariable(2),
                 Remainder(0) :+ TypeVariable(2) :+ TypeVariable(1))).

      // S (A -> B) (B -> C) compose :: S (A -> B)
      addBinding("compose",
        WordType(Remainder(0) :+ WordType(Remainder(1),  // A -> B
                                          Remainder(2))
                              :+ WordType(Remainder(2),  // B -> C
                                          Remainder(3)),
                 Remainder(0) :+ WordType(Remainder(1),  // A -> C
                                          Remainder(3)))).

      // A (A -> B) apply :: B
      addBinding("apply",
        WordType(Remainder(0) :+ WordType(Remainder(0),
                                          Remainder(1)),
                 Remainder(1))).

      // A a quote :: A (B -> B a)
      addBinding("quote",
        WordType(Remainder(0) :+ TypeVariable(1),
                 Remainder(0) :+ WordType(Remainder(2),
                                          Remainder(2) :+ TypeVariable(1)))).

      // A boolean a a if :: A a
      addBinding("if",
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
      addBinding("**",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).

      // Numeric relational operators
      addBinding("=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("!=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("<",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding("<=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding(">=",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).
      addBinding(">",
        WordType(Remainder(0) :+ NumericType :+ NumericType,
                 Remainder(0) :+ NumericType)).

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
      addBinding("includes",
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
