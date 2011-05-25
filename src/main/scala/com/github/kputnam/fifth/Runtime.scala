package com.github.kputnam.fifth

class Runtime

  // [q] infer
  // - infers the type of the quote
  //    [ 1 2 3 ] infer     ( -- x y z )
  //    [ 2 + ] infer       ( n -- n )

  // compose ( [f] [g] -- [h] )
  // curry   ( [f] x   -- [g] )
  // 

  // drop  ( x -- )
  // copy  ( x -- x )
  // clone ( x -- y )
  // over  ( x y -- x y x )
  // swap  ( x y -- y x )
  // nip   ( x y -- y )
  // pick  ( x y z -- x y z x )

  // cleave ( x #[f] -- @y )
  // - input single value and array of quotations
  // - calls each quote on the single value

  // spread ( @x #[f] -- @x )
  // - input series of objects and equal-length array of quotations
  // - calls each quotation on its corresponding object

  // napply ( @x [f] n -- @n )
  // - input series of objects with single quotation and limit
  // - calls quotation on top n objects on the stack

  // Collections
  // [ ...  list ]
  // [ ... assoc ]
  // [ ... array ]
  // collection [op] each
  // collection [op] map
  // collection [op] find-first
  // collection [op] find-last
  // collection [predicate] select
  // collection [predicate] reject
  // collection [op] reduce-left
  // collection [op] reduce-right
  // collection [op] init fold-left
  // collection [op] init fold-right
  // collection collection zip
  // collection collection [op] zip-with
  // collection collection concat
  // collection length
  // collection reverse
