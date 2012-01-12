Bee is an experimental programming language

## Fundamentals

Conceptually, all values are functions on stacks. For instance,
`0` is a function that accepts a stack input and returns a new
stack with the number zero on top.

## Minimalism



### Minimal syntax

Expressions are written in [Reverse Polish Notation](http://
en.wikipedia.org/wiki/Reverse_Polish_Notation), or postfix
notation, and have only two forms -- composition and abstraction.

Composition is denoted by terms separated with whitespace, so
`0 1` combines the function `0`, which pushes the number zero
on the top of the stack, with the function `1`. The term `0 1`
then pushes two numbers on the stack. This also applies to
terms that take values *off* the stack as well. For instance,
`+ *` combines the term `+`, which removes the two top-most
values on the stack and then pushes their sum, with the term
`*`, which removes the two top-most values (the topmost is the
result of `+`) and pushes their product.

Abstraction is denoted by terms enclosed in `[` and `]` markers,
so `[+ *]` is a higher-order function that multiplies the sum of
two numbers with a third number. The term `[+ *]` could be 
expressed JavaScript as `function(a,b,c) { return (a + b) * c; }`.

These two rules -- composition and abstraction -- describe the
entire syntax. There are no semi-colons, curly braces, infix
operators, or parentheses.

### No variable names

Operands are not identified by name; instead, they are moved to
the top of the stack (using primitives to manipulate the stack)
as needed. For instance, the primitive `dup` copies the topmost
operand so `dup *` multiplies the topmost operand by itself.

By accessing operands implicitly, we gain two things. The first
advantage is expressions are often very compact, because operand
names can be elided. The second advantage is greater flexbility
in manipulating source code. For instance, arbitrary sections of
code can be spliced without accidental variable capture.

### No keywords

The syntax of the language does not have special cases for terms
like `if`. Primitive operators are treated the same as user-defined
operators. Eventually the module system will allow primitives to
be imported using aliases so terms like `if` can be redefined.

### Single stack

Many stack-based languages like [Forth](http://en.wikipedia.org/
Forth_(programming_language)) provide the ability to transfer
data between several stacks. 

### No first-class stacks

### No heap

### No mutable state

### No closures

## Features

* Multiple return values
* Higher-order functions
* Module system
* Type inference

## Applications

## Related Links

* [Stack machine](http://en.wikipedia.org/wiki/Stack_machine)
* [Stack-oriented programming language](http://en.wikipedia.org/
Stack-oriented_programming_language)
* [concatenative.org](http://concatenative.org/)
