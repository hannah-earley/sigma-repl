module Model(module Model) where
import Numeric.Natural

data Expr = Expr (Maybe String) Expr'
data Expr' = Stop
           | Perm [Expr] [Expr]
           | Seq [Expr]
           | Noop [Expr]
           | Name String
type Context = [Expr]

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


instance Show Expr where
  show (Expr (Just name) _) = name
  show (Expr Nothing expr) = show expr
