package com.github.kputnam.bee.parser

import com.github.kputnam.bee.terms._

class ValueExpression extends scala.util.parsing.combinator.RegexParsers {

  def topLevel: Parser[QuotationTerm] =
    rep1(term) ^^ (es => QuotationTerm(es))

  def quotation: Parser[QuotationTerm] =
    "[" ~> rep(term) <~ "]" ^^ (es => QuotationTerm(es))

  def string: Parser[StringLit] =
    "\"" ~> "[^\"]*".r <~ "\"" ^^ (e => StringLit(e))

  def boolean: Parser[BooleanLit] =
    "true|false".r ^^ (e => BooleanLit("true".equals(e)))

  def numeric: Parser[NumericLit] =
    "[+-]?[0-9]+(?:\\.[0-9]*)?".r ^^ (e => NumericLit(BigDecimal(e)))

  def name: Parser[NameTerm] =
    "[^\\s\\[\\]]+".r ^^ (e => NameTerm(e))

  def term: Parser[Term] =
    quotation | string | boolean | numeric | name

}
