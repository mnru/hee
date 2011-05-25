package com.github.kputnam.fifth

class Interpreter
  // Match variables
  // $  single word
  // #  single word or quotation
  // @  any number of words or quotations

  //                          [] => []
  //                [#X #Y swap] => [#Y #X]
  //         [[@REST #TOP] call] => [@REST #TOP]
  //          [[@X] [@Y] append] => [[@X @Y]]
  //  [true [@TRUE] [@FALSE] if] => [[@TRUE] call]
  // [false [@TRUE] [@FALSE] if] => [[@FALSE] call]
  //                    [#X dup] => [#X #X]

  // words
  // quotations
  // concatenations

  // functions
  // quotation functions
  // function compositions

  // Quotation Function
  //          #X dup ==> #< #X ># #< #X >#
  //         $X $Y + ==> #<(+ $X $Y)>#
  //      $X inverse ==> #<(/ 1 $X)>#
  // $X $Y + inverse ==> #<(/ 1 (+ $X $Y))>#

  // a b c lambda ==>
  //    b squared 4 a c * * - sqrt  :> disc
  //    b negate disc + 2 a * /     :> one
  //    b negate disk - 2 a * /     :> two
  //    [ one two ];

  //
  //                               input      item   result
  // 6 5 dup [ 3 > ] call [ + ] [ * ] if
  //   5 dup [ 3 > ] call [ + ] [ * ] if         6   6
  //     dup [ 3 > ] call [ + ] [ * ] if         5   6 5
  //         [ 3 > ] call [ + ] [ * ] if       dup   6 5 5
  //                 call [ + ] [ * ] if   [ 3 > ]   6 5 5 [ 3 > ]
  //                      [ + ] [ * ] if      call   6 5 5 3 >
  //                      [ + ] [ * ] if             6 5 true
  //                            [ * ] if     [ + ]   6 5 true [ + ]
  //                                  if     [ * ]   6 5 true [ + ] [ * ]
  //                                            if   6 5 [ + ]
  //                                                 11

  // [ ...  list ]
  // [ ... array ]
  // [ ...   map ]
  // collection op each
  // collection op map
  // collection op reduce-left
  // collection op reduce-right
  // collection init op fold-left
  // collection init op fold-right
