-- private
data PhantomExpr a
  = PhantomExpr Expr
  deriving (Eq, Show)

-- private
data Expr
  = BinaryExpr    Op Expr Expr
  | UnaryExpr     Op  Expr
  | ConstantExpr  String
  deriving (Eq, Show)

-- private
data Op 
  = OpEq
  | OpGt
  | OpLt
  | OpGte
  | OpLte
  | OpNe
  | OpAdd
  | OpDiv
  | OpMod
  | OpMul
  | OpNeg
  | OpSub
  | OpAnd
  | OpNot
  | OpOr
  | OpXor
  deriving (Eq, Show)

constant :: Show a => a -> PhantomExpr a
(.+.)    :: Num a => PhantomExpr a -> PhantomExpr a -> PhantomExpr a
(.-.)    :: Num a => PhantomExpr a -> PhantomExpr a -> PhantomExpr a
(.*.)    :: Num a => PhantomExpr a -> PhantomExpr a -> PhantomExpr a
(./.)    :: Num a => PhantomExpr a -> PhantomExpr a -> PhantomExpr a
(.%.)    :: Num a => PhantomExpr a -> PhantomExpr a -> PhantomExpr a
(.&.)    :: PhantomExpr Bool -> PhantomExpr Bool -> PhantomExpr Bool
(.|.)    :: PhantomExpr Bool -> PhantomExpr Bool -> PhantomExpr Bool
(.^.)    :: PhantomExpr Bool -> PhantomExpr Bool -> PhantomExpr Bool
(.~.)    :: PhantomExpr Bool -> PhantomExpr Bool
(.<.)    :: Ord a => PhantomExpr a -> PhantomExpr a -> PhantomExpr Bool
(.>.)    :: Ord a => PhantomExpr a -> PhantomExpr a -> PhantomExpr Bool
(.<=.)   :: Ord a => PhantomExpr a -> PhantomExpr a -> PhantomExpr Bool
(.>=.)   :: Ord a => PhantomExpr a -> PhantomExpr a -> PhantomExpr Bool
(.==.)   :: Eq a => PhantomExpr a -> PhantomExpr a -> PhantomExpr Bool
(./=.)   :: Eq a => PhantomExpr a -> PhantomExpr a -> PhantomExpr Bool

constant a = PhantomExpr $ ConstantExpr (show a)
(.+.)  (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpAdd a b
(.-.)  (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpSub a b
(.*.)  (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpMul a b
(./.)  (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpDiv a b
(.%.)  (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpMod a b
(.&.)  (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpAnd a b
(.|.)  (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpOr  a b
(.^.)  (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpXor a b
(.~.)  (PhantomExpr a)                 = PhantomExpr $ UnaryExpr  OpNot a
(.<.)  (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpLt  a b
(.>.)  (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpGt  a b
(.<=.) (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpLte a b
(.>=.) (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpGte a b
(.==.) (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpEq  a b
(./=.) (PhantomExpr a) (PhantomExpr b) = PhantomExpr $ BinaryExpr OpNe  a b

-- Fails to typecheck
--   constant 1 .==. constant 'x'
