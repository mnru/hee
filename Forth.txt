
  // n EMIT         prints ASCII character   ( c -- )
  // n SPACES       prints spaces            ( n -- )
  //   CR           prints a carriage return ( -- )
  // a .            prints the current value ( a -- )
  // a .S           prints the current value ( a -- a )

  //   a b SWAP     swaps top two values  ( a b -- b a )
  //     a DUP      copies top item       ( a -- a a )
  //   a b OVER     copies second item    ( a b -- a b a )
  // a b c ROT      moves third to top    ( a b c -- b c a )
  //     a DROP     discards top          ( a -- )

  // n m +          ( n m -- sum )
  // n m -          ( n m -- difference )
  // n m *          ( n m -- product )
  // n m /          ( n m -- quotient )
  // n m %          ( n m -- remainder )
  // n m ^          ( n m -- exponentiated )
  // n ABS          ( n -- |n| )
  // n NEGATE       ( n -- -n )
  // n m MIN        ( n m -- min )
  // n m MAX        ( n m -- max )

  // n m =          ( n m -- b )
  // n m <          ( n m -- b )
  // n m >          ( n m -- b )
  // n m <=         ( n m -- b )
  // n m >=         ( n m -- b )
  //   b NOT        ( b -- b )

  // b IF ... ELSE ... THEN
  // n m DO ...    LOOP
  // m n DO ... o +LOOP
  // BEGIN ... AGAIN
  // BEGIN ... b UNTIL
  // BEGIN ... b WHILE ... REPEAT

  //*** CONTROL STACK
  // n >R   - pop and push the top of the parameter stack onto the control stack
  //   R>   - pop and push the top of the control stack onto the parameter stack
  //   R@   - copies the top of the control stack
  //   I    - copies the top of the control stack
  //   J    - copies the third item of the control stack

  //*** MEMORY
  // : f ... ;      define a word
  // a CONSTANT x   define a constant
  // VARIABLE x     declare a variable
  //        a x !   set value
  //          x @   get value
  //          x ?   print value
  //        n x +!  increment value

  //*** INTERPRETER
  // ' x            return execution token
  // x EXECUTE      execute token