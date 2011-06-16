package com.github.kputnam.fifth.parser

class Parser extends scala.util.parsing.combinator.RegexParsers {
  def topLevel: Parser[Quotation] =
    rep1(node) ^^ { case xs => Quotation(xs) }

  def quotation: Parser[Quotation] =
    "[" ~> rep(node) <~ "]" ^^ { case xs => Quotation(xs) }

  def word: Parser[Word] =
    """[^\s\[\]]+""".r ^^ { case x => Word(x) }

  def node: Parser[Node] =
    quotation | word
}