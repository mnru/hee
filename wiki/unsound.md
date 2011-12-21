Definitions

        dup : S a -> S a a
    compose : S (T -> U) (U -> W) -> S (T -> W)
          + : S num num -> S num

Evaluating `[+] dup compose`

1. `. [+] dup compose`
2. `[+] . dup compose`
3. `[+] [+] . compose`
4. `[+ +] .`

What is the type of `dup compose`?

    ----------------------------- (T-CONCAT)
     dup compose : 

The Cat type system is not compositional
http://groups.google.com/group/catlanguage/browse_thread/thread/2e20455d7a0eedb0/7924e76cbc0b7f46

    dd : D (D -> E) -> E (D -> E)
    dd = dup dip

    f : A b -> A b (C d -> C d)
    f = [id] dup dip

    g : (A b -> A b (A b -> A b)
    g = [id] dd

    fg : 
    fg = f g

    gh : 
    gh = g h

The type of `f gh` should be the same as `fg h`, but this is not the case
