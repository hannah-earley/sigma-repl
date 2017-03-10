module Output where
import Numeric.Natural
import Model

-- structural debugging

class Debug a where
  debug' :: Natural -> a -> String
  debug :: a -> String
  debug = debug' 5

instance Debug Expr where
  debug' n (Expr (Just name) x) = "<" ++ name ++ ": " ++ debug' n x ++ ">"
  debug' n (Expr Nothing x) = debug' n x

instance Debug Expr' where
  debug' _ Stop = "_"
  debug' n (Perm xs xs') = "<pi " ++ debugs (n-1) xs ++ " | " ++ debugs (n-1) xs' ++ " pi>"
  debug' n (Seq xs) = "(" ++ debugs (n-1) xs ++ ")"
  debug' n (Noop xs) = "{" ++ debugs (n-1) xs ++ "}"
  debug' _ (Name n) = '$' : n

debugs :: Natural -> [Expr] -> String
debugs n = unwords . map (debug' n)


-- structural visualisation

instance Show Expr where
  show (Expr (Just name) _) = name
  show (Expr Nothing expr) = show expr

instance Show Expr' where
  show Stop = "_"
  show (Perm xs xs') = "<pi " ++ prints xs ++ " | " ++ prints xs' ++ " pi>"
  show (Seq xs) = "(" ++ prints xs ++ ")"
  show (Noop xs) = printd xs
  show (Name n) = '$' : n

-- helper function
prints :: [Expr] -> String
prints = unwords . map show

-- special printing sugar for datatypes...
printd :: [Expr] -> String

  -- lists
printd [Expr (Just "Nil") _, _] = "[]"
printd x@[Expr (Just "Cons") _, _] = "[" ++ s ++ "]"
  where
    s = unwords . printl . Expr Nothing $ Noop x
    printl :: Expr -> [String]
    printl (Expr _ (Noop [Expr (Just "Cons") _, Expr _ (Noop [x, y])])) = show x : printl y
    printl (Expr _ (Noop [Expr (Just "Nil") _, _])) = []
    printl x = [". " ++ show x]

  -- numbers
printd [Expr (Just "Zero") _, _] = "#0"
printd [Expr (Just "Succ") _, n] = printn 1 n
  where
    printn :: Natural -> Expr -> String
    printn m (Expr _ (Noop [Expr (Just "Zero") _, _])) = '#' : show m
    printn m (Expr _ (Noop [Expr (Just "Succ") _, n])) = printn (m+1) n
    printn m x = "{#" ++ show m ++ " . " ++ show x ++ "}"

  -- fallback
printd xs = "{" ++ prints xs ++ "}"
