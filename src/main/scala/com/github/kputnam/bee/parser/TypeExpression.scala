package com.github.kputnam.bee.parser

import com.github.kputnam.bee.types._

class TypeExpression extends scala.util.parsing.combinator.RegexParsers {

  // @todo: Lookup names in SymbolTable
  def topLevel: Parser[Type] =
    ( "bitmap"  ^^^ BitmapType
    | "bool"    ^^^ BooleanType
    | "byte"    ^^^ ByteType
    | "char"    ^^^ CharacterType
    | "num"     ^^^ NumericType
    | "str"     ^^^ StringType
    | "any"     ^^^ TopType
    | "nothing" ^^^ BottomType
    | word )

  // @todo: Lookup names in SymbolTable
  def argument: Parser[Type] =
    ( "bitmap"  ^^^ BitmapType
    | "bool"    ^^^ BooleanType
    | "byte"    ^^^ ByteType
    | "char"    ^^^ CharacterType
    | "num"     ^^^ NumericType
    | "str"     ^^^ StringType
    | "any"     ^^^ TopType
    | "nothing" ^^^ BottomType
    | variable
    | word )

  def identifier: Parser[String] =
    "[^\\s]+"

  def word: Parser[WordType] =
    "(" ~ stack ~ rep(argument) ~ ("->" | "→") ~ stack ~ rep(argument) ~ ")" ^^ {
      case _ ~ a ~ as ~ _ ~ b ~ bs ~ _ =>
        WordType((a /: as)((s, τ) => s :+ τ),
                 (b /: bs)((s, τ) => s :+ τ)) }

  def product: Parser[String] =
    "(" ~ rep1sep(argument, ("x" | "⨯")) ~ ")" ~ "product" ^^^ ""

  def record: Parser[String] =
    "(" ~ rep1sep(identifier ~ ":" ~ argument, ("x" | "⨯")) ~ ")" ~ "record" ^^^ ""

  def variant: Parser[String] =
    "(" ~ rep1sep(identifier ~ ":" ~ argument, "|") ~ ")" ~ "variant" ^^^ ""

  def sum: Parser[String] =
    "(" ~ rep1sep(argument, "|") ~ ")" ~ "sum" ^^^ ""

  def stack: Parser[StackType] =
    "[ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩABCDEFGHIJKLMNOPQRSTUVWXYZ]'*".r ^^ (x => Remainder.fromName(x))

  def variable: Parser[Variable] =
    "[αβγδεζηθικλμνξοπρςστυφχψωabcdefghijklmnopqrstuvwxyz]'*".r ^^ (x => Variable.fromName(x))

}
