package com.github.kputnam.bee.parser

import com.github.kputnam.bee.nodes._

class ValueExpression extends scala.util.parsing.combinator.RegexParsers {

  def topLevel: Parser[RootNode] =
    rep1(node) ^^ (xs => RootNode(xs))

  def quotation: Parser[QuotationNode] =
    "[" ~> rep(node) <~ "]" ^^ (xs => QuotationNode(xs))

  def string: Parser[StringLit] =
    "\"" ~> "[^\"]*".r <~ "\"" ^^ (x => StringLit(x))

  def boolean: Parser[BooleanLit] =
    "true|false".r ^^ (x => BooleanLit("true".equals(x)))

  def numeric: Parser[NumericLit] =
    "[+-]?[0-9]+(?:\\.[0-9]*)?".r ^^ (x => NumericLit(BigDecimal(x)))

  def word: Parser[WordNode] =
    "[^\\s\\[\\]]+".r ^^ (x => WordNode(x))

  def node: Parser[AbstractNode] =
    quotation | string | boolean | numeric | word

}
