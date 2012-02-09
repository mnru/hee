Bee is an experimental (read: not for production use) programming
language.

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
operators. Later, a module system will allow you to import operators
using alternate names, or to exclude operators from being imported.

This means you can name your operators freely; certain names are
not reserved for the language.

### No mutable state

### No heap

### No lexical scope

### No closures

Closures provide a way for functions to reference variables outside
of its lexical scope. Remember we don't have mutable state (variables)
nor do we have lexical scope. But we do have function composition.

Consider implementing `(a list) (a → bool) filter → (a list)` using
`(a list) b (a → b → b) foldl → b`. Since `filter` preserves the list
type, we need to use the `a → bool` argument of `filter` to build
the `(a → (a list) → (a list))` argument of `foldl`.

If we passed `foldl` a function like `[swap dup _ [cons] [pop] if]`,
where `_` is the `a → bool` predicate function passed to `filter`,
then we `cons` the item onto a list if the predicate is satisfied,
and discard it otherwise. So how do we fill-in the `_`? We know that
function is somewhere below on the stack; but digging around down
is not only inefficient (copying a function several levels below),
but its not safe. The function passed to `foldl` knows only that
the stack top is `a (a list)`, there's no information about the rest
of the stack.

Instead, we can solve this problem using function composition. We
assume `_`, the `a → bool` operand is the top of the stack. The
function we want to build starts with `[swap dup]`, and to add `_`
to the end of it, we simply `swap compose`. Now we have `[swap dup
_]`. We tack the rest on with `[[cons] [pop] if] compose`.

It turns out we can always manually "absorb" values in this manner
using function composition, rather than having a compiler close over
references to free variables automatically.

### Single stack

Many stack-based languages like [Forth](http://en.wikipedia.org/
Forth_%28programming_language%29) provide the ability to transfer
data between different stacks. This really comes in handy to move
operands temporarily out of the way.

During this early stage in the development of the language, only
one stack is made available. Hopefully this affords the compiler
more freedom to target diverse platforms (which may or may not
provide access to multiple stacks).

## Features

* Multiple return values
* Higher-order functions
* Module system
* Type inference
* Class polymorphism

## Applications

## Related Links

* [Stack machine](http://en.wikipedia.org/wiki/Stack_machine)
* [Stack-oriented programming language](http://en.wikipedia.org/Stack-oriented_programming_language)
* [concatenative.org](http://concatenative.org/)
* [My History with Forth Stack Machines](http://www.yosefk.com/blog/my-history-with-forth-stack-machines.html)
* [Understanding What It's Like to Program in Forth](http://prog21.dadgum.com/33.html)
