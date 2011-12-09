Writing functions can be difficult in a concatenative language. For instance, try
defining `map` such that `null 1 cons 2 cons 3 cons [1 +] map` evaluates to the
list `null 2 cons 3 cons 4 cons`. One approach is to iteratively invoke functions,
working backwards from the desired result to the beginning, in the REPL like

    null
    1 1 + cons 
    2 1 + cons
    3 1 + cons

It might be useful if the REPL kept a transcript of these commands so they could
be dumped into an editor. 
