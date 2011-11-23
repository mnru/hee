
Building up an expression

                [ 3 ] :: A → A num
              [ 3 9 ] :: A → A num num
          [ 3 9 rot ] :: A β → A num num β
    [ 3 9 rot apply ] :: A (A num num → B) → B

Note we're binding two values to `num-num`

    -- num-num-op applies a given operation to the stack
    -- after pushing two nums to the top of the stack
    num-num    = [ 3 9 ]
    num-num-op = [ num-num rot apply ]

The number of input and output parameters is not fixed, it
depends on the parameter given to `num-num-op`!

    -- id has no effect on the stack
     [id] num-num-op :: A → A num num
      [+] num-num-op  = 3 9

    -- dup copies the top of the stack
    [dup] num-num-op :: A → A num num num
      [+] num-num-op  = 3 9 9

    -- + replaces the top two nums with a single num
      [+] num-num-op :: A → A num
      [+] num-num-op  = 12

    -- if :: bool a a → a
     [if] num-num-op :: A bool → A num

Simulating pairs

    fst = [ true if]
    snd = [false if]

    [fst] num-num-op = 3
    [snd] num-num-op = 9

