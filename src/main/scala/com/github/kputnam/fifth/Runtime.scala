package com.github.kputnam.fifth

/*
import collection.immutable.{Queue, Stack}
import parser.{Word, Quotation, Node}

object Runtime {
  def empty: Runtime =
    Runtime(Queue.empty, Stack.empty)
}

case class Runtime(input: Queue[Node], output: Stack[Node]) {

  // Add a single input token
  def read(node: Node): Runtime =
    new Runtime(input.enqueue(node), output)

  // Read and interpret a single input token
  def step: Runtime = input.dequeue match {
    case (Word("id"),      rest) => this
    case (Word("halt"),    rest) => Runtime.empty
    case (Word("pop"),     rest) => pop(rest)
    case (Word("dup"),     rest) => dup(rest)
    case (Word("swap"),    rest) => swap(rest)
    case (Word("compose"), rest) => compose(rest)
    case (Word("apply"),   rest) => apply(rest)
    case (Word("quote"),   rest) => quote(rest)
    case (word,            rest) => new Runtime(rest, output.push(word))
  }

  // A a pop :: A
  private def pop(input: Queue[Node]) =
    new Runtime(input, output.pop)

  // A a dup :: A a a
  private def dup(input: Queue[Node]) =
    new Runtime(input, output.push(output.top))

  // A b a swap :: A a b
  private def swap(input: Queue[Node]) = {
    val (a, outputB) = output.pop2
    val (b, outputC) = outputB.pop2
    new Runtime(input, outputC.push(a).push(b))
  }

  // A (A -> B) apply :: B
  private def apply(input: Queue[Node]): Runtime = {
    val (a: Quotation, outputB) = output.pop2
    new Runtime(Queue(a.nodes : _*) ++ input, outputB)
  }

  // A a quote :: A (B -> B a)
  private def quote(input: Queue[Node]): Runtime = {
    val (a, outputB) = output.pop2
    new Runtime(input, outputB.push(Quotation(List(a))))
  }

  // A (B -> C) (C -> D) compose :: A (B -> D)
  private def compose(input: Queue[Node]): Runtime = {
    val (a: Quotation, outputB) = output.pop2
    val (b: Quotation, outputC) = outputB.pop2
    new Runtime(input, outputC.push(new Quotation(b.nodes ++ a.nodes)))
  }

}
*/
