type Id = String

-- Terms are either a composition of one function with another term, or
-- a higher-order function (a string of composed functions)
data Term = TmCompose Term Id
          | TmQuote Term
          | TmEmpty
  deriving (Eq, Show)

-- Kinds classify types as either a monomorphic value type (a nullary type
-- constructor), a unary type constructor (type => type), or a stack
data Kind = KiStack
          | KiType
          | KiCons Kind Kind
  deriving (Eq, Show)

-- Types have the kind KiType and are distinguished from Stack because
-- they can be used to describe first-class values.
data Type = TyVariable Id Kind
          | TyConstant Id Kind
          | TyApplication Type Type
          | TyStack Stack
  deriving (Eq, Show)

-- Stacks have the kind KiStack and are distinguished from Type because
-- they cannot be used to describe first-class values.
data Stack = StEmpty
           | StBottom Id
           | StPush Stack Type
  deriving (Eq, Show)

showTerm :: Term -> String
showTerm TmEmpty                = ""
showTerm (TmQuote t)            = "[" ++ showTerm t ++ "]"
showTerm (TmCompose TmEmpty u)  = u
showTerm (TmCompose t u)        = showTerm t ++ " " ++ u

showKind :: Kind -> String
showKind KiStack                    = "@"
showKind KiType                     = "*"
showKind (KiCons k@(KiCons _ _) k') = "(" ++ showKind k ++ ") => " ++ showKind k'
showKind (KiCons k k')              = showKind k ++ " => " ++ showKind k'

showType :: Type -> String
showType (TyVariable id k)    = id
showType (TyConstant id k)    = id
showType (TyApplication t t') = "(" ++ showType t ++ ") " ++ showType t'
showType (TyStack s)          = "(" ++ showStack s ++ ")"

showStack :: Stack -> String
showStack StEmpty       = "|"
showStack (StBottom id) = id
showStack (StPush s s') = showStack s ++ "> " ++ showType s'

-- Primitive types
tInt  = TyConstant "int"  KiType
tReal = TyConstant "real" KiType
tChar = TyConstant "char" KiType
tUnit = TyConstant "()"   KiType
tPair = TyConstant "(,)"  (KiCons KiType (KiCons KiType KiType))
tList = TyConstant "[]"   (KiCons KiType (KiCons KiType KiType))
tWord = TyConstant "(->)" (KiCons KiStack KiStack)

mkWord :: Stack -> Stack -> Type
mkWord inp out = TyApplication (TyApplication tWord (TyStack inp)) (TyStack out)


