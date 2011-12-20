# Type Inference

## Term Inference

The K combinator has the type ∀α.∀β.α→β→α. In English this reads, for all α and β, K is a function that maps an α-value and β-value to an α-value. Does this type signature tell us anything about what K does?

Without certain escape hatches, our implementation of K cannot possibly return an α-value other than the one it was given. Because that α could be any type, the operations we can perform on our α-value are restricted to operations common to all types. Some languages have a *top type*, to which all values belong, like `java.lang.Object` that defines operations like `toString`, `hashCode`, etc. We ultimately want to produce an α-value, the operations on `java.lang.Object` are unable to do that for the same reasons that we are unable.

Of course most languages *do* have escape hatches. For instance, in Java we could return `null`, which is a member of all types. We could instead fail to return a value at all by throwing an exception. If the language supports it, we might use runtime information about α to construct non-trivial values like "HI" when α is `String` or 100 when α is `Integer`.

But modulo those escape hatches, we can deduce that K must return its first argument. So what happens with the β-value? It depends on the evaluation strategy. In a *lazily evaluated* expression, subexpressions are only evaluated when their result is needed. For an example, consider the expression `f || g` in which `g` is never evaluated when `f` evaluates to true. In a *strictly evaluated* expression, subexpressions are always evaluated. When function application is strictly evaluated, an expression like `or(f, g)` first evaluates the subexpressions `f` and `g` before evaluting `or`. Even if the definition of `or` was something like `return f ? f : g`, the function `or` has no ability to prevent the evaluation of `g`.

