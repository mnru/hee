package com.github.kputnam.bee.parser

import com.github.kputnam.bee.types._

class TypeExpression extends scala.util.parsing.combinator.RegexParsers {

  def topLevel: Parser[AbstractType] =
    ( "bitmap" ^^^ BitmapType
    | "bool"   ^^^ BooleanType
    | "byte"   ^^^ ByteType
    | "char"   ^^^ CharacterType
    | "num"    ^^^ NumericType
    | "str"    ^^^ StringType
    | word )

  def word: Parser[WordType] =
    "(" ~ stack ~ rep(argument) ~ "->" ~ stack ~ rep(argument) ~ ")" ^^ {
      case _ ~ a ~ as ~ _ ~ b ~ bs ~ _ =>
        WordType((a /: as)((s, t) => s :+ t),
                 (b /: bs)((s, t) => s :+ t)) }

  def argument: Parser[AbstractType] =
    ( "bitmap" ^^^ BitmapType
    | "bool"   ^^^ BooleanType
    | "byte"   ^^^ ByteType
    | "char"   ^^^ CharacterType
    | "num"    ^^^ NumericType
    | "str"    ^^^ StringType
    | variable
    | word )

  def stack: Parser[StackType] =
    "[ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩABCDEFGHIJKLMNOPQRSTUVWXYZ]'*".r ^^ (x => Remainder.fromString(x))

  def variable: Parser[TypeVariable] =
    "[αβγδεζηθικλμνξοπρςστυφχψωabcdefghijklmnopqrstuvwxyz]'*".r ^^ (x => TypeVariable.fromString(x))

}
