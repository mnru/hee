package com.github.kputnam.fifth.parser

class Parser extends scala.util.parsing.combinator.RegexParsers {
  def topLevel: Parser[Quotation] =
    rep1(node) ^^ (x => Quotation(x))

  def quotation: Parser[Quotation] =
    "[" ~> rep(node) <~ "]" ^^ (x => Quotation(x))

  def string: Parser[StringLit] =
    "\"" ~> "[^\"]*".r <~ "\"" ^^ (x => StringLit(x))

  def boolean: Parser[BooleanLit] =
    "true|false".r ^^ { case "true"  => BooleanLit(true)
                        case "false" => BooleanLit(false) }

  def numeric: Parser[NumericLit] =
    "[+-]?[0-9]+(?:\\.[0-9]*)?".r ^^ (x => NumericLit(BigDecimal(x)))

  def word: Parser[Word] =
    "[^\\s\\[\\]]+".r ^^ (x => Word(x))

  def node: Parser[Node] =
    quotation | string | boolean | numeric | word
}