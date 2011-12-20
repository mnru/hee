Concatenative languages have an interesting property that allows programmers
to extract arbitrary fragments of code into functions. This might not sound
like much, so let's see an example.

    var a = new Point(4, 9, 3)
      , b = new Point(5, 0, 1)
      , ab = Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2) + Math.pow(a.z - b.z, 2))
      , ba = Math.sqrt(Math.pow(b.x - a.x, 2) + Math.pow(b.y - a.y, 2) + Math.pow(b.z - a.z, 2));

    console.log(ab == ba, ab, ba);

Seems like we could improve this by factoring the common code into a function.

    var distance = function(a, b) {
      return Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2) + Math.pow(a.z - b.z, 2)); }

    var a = new Point(4, 9, 3)
      , b = new Point(5, 0, 1)
      , ab = distance(a, b)
      , ba = distance(b, a);

    console.log(ab == ba, ab, ba);

We might go one step further by factoring the thrice repeated `(a._ - b._)**2` into a function.

    var diffsq   = function(m, n) { return (m - n) ** 2;
        distance = function(a, b) {
          return Math.sqrt(diffSq(a.x, b.x) + diffSq(a.y, b.y) + diffSq(a.z, b.z)); }

Compare this to the analogous process in a concatenative language

    4 9 3 point                 // (4,9,3)
    5 0 1 point                 // (4,9,3) (5,0,1)
    [dup .x] dip swap           // (4,9,3) (5,0,1) 4
    [dup .x] dip swap           // (4,9,3) (5,0,1) 4 5
    - dup *                     // (4,9,3) (5,0,1) 1

    [[dup .y] dip] dip swap
      // (4,9,3) (5,0,1) 1      . [[dup .y] dip] dip swap

    [[dup .y] dip] dip swap     // (4,9,3) (5,0,1) 9 1
    [[dup .y] dip] dip swap     // (4,9,3) (5,0,1) 9 0 1
    [- dup *] dip               // (4,9,3) (5,0,1) 3 1
