instance Show Kind where
  show = showKind

instance Show Type where
  show = showType

instance Show Stack where
  show = showStack

showKind :: Kind -> String
showKind KStack = "@"
showKind KType  = "*"
showKind (KConstructor k@(KConstructor _ _) k')
                = "(" ++ showKind k ++ ") => " ++ showKind k'
showKind (KConstructor k k')
                = showKind k ++ " => " ++ showKind k'

showId :: Id -> String -> String
showId id alphabet = (alphabet !! n) : (replicate k '\'')
  where k = id `div` length alphabet
        n = id `mod` length alphabet

showVar :: Id -> Kind -> String
showVar id KStack = showId id "ABCDEFGHIJKLMNOPQRTSUVWXYZ"
showVar id _      = showId id "abcdefghijklmnopqrtsuvwxyz"

showType :: Type -> String
showType (TStack s)          = showStack s
showType (TConstructor id k) = id
showType (TVariable id k)    = showVar id k
showType (TForall id k ps t) = "∀" ++ (showVar id k) ++ ". " ++ showType t
showType (TApplication (TApplication f i) o) | f == tFunc = "(" ++ showType i ++ " → " ++ showType o ++ ")"
showType (TApplication (TApplication f i) o) | f == tPair = "(" ++ showType i ++ ", " ++ showType o ++ ")"
showType (TApplication f i)                  | f == tList = "[" ++ showType i ++ "]"
showType (TApplication f x)                               = "(" ++ showType f ++ " " ++ showType x ++ ")"

showStack :: Stack -> String
showStack SEmpty       = "∅"
showStack (SPush s s') = showStack s ++ " " ++ showType s'
showStack (SBottom id) = showVar id KStack
