  
  Numerals
    0 = λf.λx.x
    1 = λf.λx.f x
    2 = λf.λx.f (f x)
    3 = λf.λx.f (f (f x))
     ...
    n = λf.λx.f^n x

    plus  = λm.λn.λf.λx.m f (n f x)
    succ  = λn.λf.λx.f (n f x)
    mult  = λm.λn.λf.λx.m (n f) x
    exp   = λm.λn.n (mult m) 1
    pred  = λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
    sub   = λm.λn.(n pred) m
    zero? = λn.n (λx.F) T

  Booleans
    T = λt.λf.t 
    F = λt.λf.f

    (= (T 1 0) 1)
    (= (F 1 0) 0)

  Pairs
    CONS = λa.λb.λx.x a b
    FST  = λp.p (λa.λb.a)
    SND  = λp.p (λa.λb.b)
