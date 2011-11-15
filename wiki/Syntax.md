
  S T                     pop : S
  S T                     dup : S T T
  S U                   quote : S (T -> T U)
  S (S -> T)            apply : T
  S (T -> U) (U -> V) compose : S (T -> V)

  Terms
    t :=
      [word]    abstraction
      t word    application
      t t       ...?

      -- Move from stack to heap (non-GC?)
      t name define

      -- Reference heap value
      name

