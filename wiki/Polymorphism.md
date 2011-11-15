
  Ad-Hoc Polymorphism

  - Nominal (Overloading)
    - each instance has separate definition
    - makes first-class functions harder

  - Structural (Typecase)
    - 

  - Coercion
    - 2.0 + 3 (coerce both to Float)
    - 3 + 2.0 (coerce both to Float)
    - implicit coercion
        FloatToInt(f: Float): Int
        IntToFloat(i: Int): Float

  -- Interesting
       [+] twice : int int int -> int
  [negate] twice : int -> int

  Enumeration
    1 2 ..

  -- Binders
  --   types:  ∀ ∃
  --   values: λ ∏ 

  -- The type of any value
  Top = ∃α.α

  class α Eq
    eq : α α Boolean Function2

  instance Int Eq
    eq : Int Int Boolean Function2
    eq ≡  int-eq

  instance (α Eq) => (α List) Eq
    eq : (α List) (α List) Boolean Function2
    eq ≡  
    xs ys eq ≡ [ xs ys [eq] zip-with ]
               [ false ]
               xs length =
               ys length =
               and if

  -- (α Eq, β Eq) is a prerequisite
  instance (α Eq, β Eq) => (α β Pair) Eq
    eq : (α β Pair) (α β Pair) Boolean Function2
    eq ≡  ...
    x y eq ≡ x fst y fst =
             x snd y snd =
             and

  -- Eq is a prerequisite/superclass of Ord
  class α Eq => α Ord
    lt : α α Boolean Function2


  -- Dictionary passing style
  sort : ∀α.Ord α -> [α] -> [α]

  let x = [1 .. 10]
   in eq x (sort x)

  let x = [1 .. 10]
   in (tcEqList tcEqInt).eq x (sort (tcOrdList tcOrdInt) x)
