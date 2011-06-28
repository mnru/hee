package com.github.kputnam.fifth

import types._

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
        WordType(StackType(RestVariable(0),
                           TypeVariable(1)),
                 StackType(RestVariable(0)))).

      // T a dup :: T a a
      addBinding("dup",
        WordType(StackType(RestVariable(0),
                           TypeVariable(1)),
                 StackType(RestVariable(0),
                           TypeVariable(1),
                           TypeVariable(1)))).

      // T id :: T
      addBinding("id",
        WordType(StackType(new RestVariable(0)),
                 StackType(new RestVariable(0)))).

      // T a b swap :: T b a
      addBinding("swap",
        WordType(StackType(RestVariable(0),
                           TypeVariable(1),
                           TypeVariable(2)),
                 StackType(RestVariable(0),
                           TypeVariable(2),
                           TypeVariable(1)))).

      // S (A -> B) (B -> C) compose :: S (A -> B)
      addBinding("compose",
        WordType(StackType(RestVariable(0),
                           WordType(StackType(RestVariable(1)),  // A -> B
                                    StackType(RestVariable(2))),
                           WordType(StackType(RestVariable(2)),  // B -> C
                                    StackType(RestVariable(3)))),
                 StackType(RestVariable(0),
                           WordType(StackType(RestVariable(1)),  // A -> C
                                    StackType(RestVariable(3)))))).

      // A (A -> B) apply :: B
      addBinding("apply",
        WordType(StackType(RestVariable(0),                       // A
                           WordType(StackType(RestVariable(0)),   // A -> B
                                    StackType(RestVariable(1)))),
                 StackType(RestVariable(1)))).                    // B

      // A a quote :: A (B -> B a)
      addBinding("quote",
        WordType(StackType(RestVariable(0),
                           TypeVariable(1)),                        // a
                 StackType(RestVariable(0),
                           WordType(StackType(RestVariable(2)),     // B -> B a
                                    StackType(RestVariable(2),
                                              TypeVariable(1)))))).

      // A boolean a a if :: A a
      addBinding("if",
        WordType(StackType(RestVariable(0),
                           BooleanType,
                           TypeVariable(1),
                           TypeVariable(1)),
                 StackType(RestVariable(0),
                           TypeVariable(1)))).

      // Numeric operators
      addBinding("+",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("-",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("*",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("/",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("%",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("**",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).

      // Numeric relational operators
      addBinding("=",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("!=",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("<",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("<=",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding(">=",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding(">",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).

      // Bitwise operators
      addBinding("&",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("^",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("~",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("|",
        WordType(StackType(RestVariable(0),
                           NumericType,
                           NumericType),
                 StackType(RestVariable(0),
                           NumericType))).

      // Logical operators
      addBinding("&",
        WordType(StackType(RestVariable(0),
                           BooleanType,
                           BooleanType),
                 StackType(RestVariable(0),
                           BooleanType))).
      addBinding("^",
        WordType(StackType(RestVariable(0),
                           BooleanType,
                           BooleanType),
                 StackType(RestVariable(0),
                           BooleanType))).
      addBinding("~",
        WordType(StackType(RestVariable(0),
                           BooleanType),
                 StackType(RestVariable(0),
                           BooleanType))).
      addBinding("|",
        WordType(StackType(RestVariable(0),
                           BooleanType,
                           BooleanType),
                 StackType(RestVariable(0),
                           BooleanType))).

      // String operators
      addBinding("+",
        WordType(StackType(RestVariable(0),
                           StringType,
                           StringType),
                 StackType(RestVariable(0),
                           StringType))).
      addBinding("take",
        WordType(StackType(RestVariable(0),
                           StringType,
                           NumericType),
                 StackType(RestVariable(0),
                           StringType))).
      addBinding("drop",
        WordType(StackType(RestVariable(0),
                           StringType,
                           NumericType),
                 StackType(RestVariable(0),
                           StringType))).
      addBinding("length",
        WordType(StackType(RestVariable(0),
                           StringType),
                 StackType(RestVariable(0),
                           NumericType))).
      addBinding("empty?",
        WordType(StackType(RestVariable(0),
                           StringType),
                 StackType(RestVariable(0),
                           BooleanType))).
      addBinding("includes",
        WordType(StackType(RestVariable(0),
                           StringType,
                           StringType),
                 StackType(RestVariable(0),
                           BooleanType)))
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