Similar syntax to Factor. To avoid the need for look-ahead, we put the
macro token at the start. Further "(XML" is probably better than "XML(".

These macros are user-defined and must be imported. 

    "universe" [XML <b>Hello, _</b>]

    2 TUPLE[1 _ 3]

    LIST[1 2 3 4]

    MAP["A" 1
        "B" 2]
