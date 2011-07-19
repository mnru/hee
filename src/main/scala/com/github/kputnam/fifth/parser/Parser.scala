package com.github.kputnam.bcat.parser

import com.github.kputnam.bcat.nodes._

class Parser extends scala.util.parsing.combinator.RegexParsers {
  def topLevel: Parser[QuotationNode] =
    rep1(node) ^^ (x => QuotationNode(x))

  def quotation: Parser[QuotationNode] =
    "[" ~> rep(node) <~ "]" ^^ (x => QuotationNode(x))

  def string: Parser[StringLit] =
    "\"" ~> "[^\"]*".r <~ "\"" ^^ (x => StringLit(x))

  def boolean: Parser[BooleanLit] =
    "true|false".r ^^ { case "true"  => BooleanLit(true)
                        case "false" => BooleanLit(false) }

  def numeric: Parser[NumericLit] =
    "[+-]?[0-9]+(?:\\.[0-9]*)?".r ^^ (x => NumericLit(BigDecimal(x)))

  def word: Parser[WordNode] =
    "[^\\s\\[\\]]+".r ^^ (x => WordNode(x))

  def node: Parser[AbstractNode] =
    quotation | string | boolean | numeric | word
}
