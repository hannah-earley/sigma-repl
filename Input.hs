{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Input
( loadprog
, loadstr
) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Numeric.Natural
import Data.Either
import Data.Tuple

import Data.Functor
import Control.Applicative
import Control.Monad

import System.IO (readFile)
import System.IO.Error (isDoesNotExistError)
import System.Directory (withCurrentDirectory)
import System.FilePath.Posix (splitFileName, (<.>))
import Control.Exception (catchJust)

import Model (Expr(..))
import Module (Group(..))

--- parser type

data Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f p = Parser $ \inp -> do (v, inp') <- runParser p inp
                                 return (f v, inp')

instance Applicative Parser where
  pure a = Parser $ \inp -> [(a, inp)]
  p <*> q = Parser $ \inp -> do (f, inp') <- runParser p inp
                                (v, inp'') <- runParser q inp'
                                return (f v, inp'')

instance Alternative Parser where
  empty = Parser $ \_ -> []
  p <|> q = Parser $ \inp -> (runParser p inp ++ runParser q inp)

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \inp -> do (v, inp') <- runParser p inp
                                runParser (f v) inp'

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

--- combinators

pair :: Parser a -> Parser b -> Parser (a, b)
pair p q = [(x,y) | x <- p, y <- q]

-- many is defined by Applicative
many1 :: Parser a -> Parser [a]
many1 p = [x:xs | x <- p, xs <- many p]

largest :: Parser a -> Parser a
largest p = Parser $ \inp -> case runParser p inp of
                            []     -> []
                            (x:xs) -> [x]

smallest :: Parser a -> Parser a
smallest p = Parser $ \inp -> case runParser p inp of
                            [] -> []
                            ls -> [last ls]

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = largest $ p <|> q

--- chars and strings

item :: Parser Char
item = Parser $ \inp -> case inp of
                        []     -> []
                        (x:xs) -> [(x,xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = [x | x <- item, p x]

char x = sat (== x)

chars :: [Char] -> Parser Char
chars cs = foldl (<|>) mzero $ map char cs

string :: String -> Parser String
string ""     = return ""
string (x:xs) = (:) <$> char x <*> string xs

--- brackets

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket l p r = do { l ; x <- p ; r ; return x }

--- lexing

ignore :: Parser a -> Parser ()
ignore = (const () <$>)

spaces :: Parser ()
spaces = [() | _ <- many1 $ chars " \t\n\f\r\v"]

comment :: Parser ()
comment = smallest . ignore
                   $ bracket (string "(*")
                             (many $ comment <||> ignore item)
                             (string "*)")

junk :: Parser ()
junk = ignore . many $ spaces <||> comment

parse :: Parser a -> Parser a
parse = (junk >>)

token :: Parser a -> Parser a
token p = do { x <- p ; junk ; return x}

--- symbols

type ID = String

symbol :: String -> Parser ID
symbol = token . string

ident :: Parser ID
ident = largest $ liftM2 (:) start (many other)
  where
    reserved = "\t\n\f\r\v .#@<>(){}[]`:\""
    reserved' = reserved ++ "0123456789"

    start = sat $ not . flip elem reserved'
    other = sat $ not . flip elem reserved

identifier :: [String] -> Parser ID
identifier ks = token [x | x <- ident, not (x `elem` ks)]

--- string literals

escape :: Char -> Map Char Char -> Parser Char
escape ec ecs = do char ec
                   c <- item
                   return $ M.findWithDefault c c ecs

stresc :: Parser Char
stresc = escape '\\' $ M.fromList
  [('a', '\a'), ('b', '\b'), ('f', '\f'), ('n', '\n')
  ,('r', '\r'), ('t', '\t'), ('v', '\v')]

strlit :: Char -> Parser String
strlit c = let schar = stresc <||> sat (/= c)
           in bracket (char c) (many schar) (char c)

litstr :: Char -> Parser String
litstr = token . strlit

--- natural number literals

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

nat :: Parser Natural
nat = read <$> largest (many1 digit)

--- common

kws = ["inherit", "inherit*", "bequeath", "group",
       "group*", "def", "def*", "perm", "perm*"]

variable :: Parser ID
variable = identifier kws

labelgen :: (ID -> e) -> (ID -> m -> e) -> Parser m -> Parser e
labelgen lf af ae = do l <- variable
                       (<||>) (symbol "@" >> af l <$> ae)
                              (return $ lf l)

paren p = bracket (symbol "(") p (symbol ")")
brack (l:r:"") p = bracket (symbol $ l:"") p (symbol $ r:"")
env n p = brack "()" $ symbol n >> p

--- sigma expressions

type Expr' = Expr ID ID

expr :: Parser Expr'
expr = largest atom
  where

    atom = stop <||> perm <||> app <||> label <||> ref <||> lit

    stop = symbol "." <|> symbol "#" >> return Stop

    perm = brack "<>" $ do (sl, l) <- perml
                           symbol ":"
                           (r, sr) <- permr
                           guard (sl == sr)
                           return $ Perm l r
      where
        perml = pair label (many expr)
        permr = pair (many expr) label

    app = paren $ Seq <$> many expr

    label = labelgen Label As expr

    ref = string "`" >> Ref <$> variable

    -- literals

    lit = lnat <||> ldata <||> llist

    ldata = brack "{}" $ wrap <$> many expr

    llist = brack "[]" $ foldr co ni <$> many expr
      where
        co x xs = wrap [Ref "cons", wrap [x, xs]]
        ni = wrap [Ref "nil", Stop]

    lnat = wnat <$> token nat
      where
        wnat 0 = wrap [Ref "zero", Stop]
        wnat n = wrap [Ref "succ", wnat (n - 1)]

    wrap es = Seq $ [Stop] ++ es ++ [Stop]

--- program terms

data Term = Inherit FilePath (Either () [(ID, ID)])
          | Bequeath [(ID, ID)]
          | TermGroup [Term]
          | Define Bool (ID, ID) Expr'
          | Raw Expr'

deriving instance Show Expr' => Show Term

terms :: Parser [Term]
terms = largest $ many term
  where

    term = largest atom

    atom = inherit <||> bequeath <||> group <||> def <||> raw

    inherit = all <||> some
      where
        all = env "inherit*" $ flip Inherit (Left ()) <$> path
        some = env "inherit" $
            liftM2 Inherit path (Right <$> many promoter)

    bequeath = env "bequeath" $ Bequeath <$> many promoter

    group = env "group" $ TermGroup <$> many term

    def = def' True "*" <||> def' False "" <||>
         perm' True "*" <||> perm' False ""
      where
        def' b s = env ("def" ++ s) $
            liftM2 (Define b) promoter expr

        perm' b s = env ("perm" ++ s) $
               do (sl, l) <- perml
                  (r, sr) <- permr
                  guard (sl == sr || (snd sr == fst sr)
                                    && fst sr == fst sl)
                  return . Define b sl $ Perm l r

        perml = paren $ pair promoter (many expr)
        permr = paren $ pair (many expr) promoter

    raw = Raw <$> expr

    path = litstr '"' <||> litstr '\''

    promoter = labelgen (\l -> (l, l)) (,) variable

--- program translation

type ExprGroup = Group (Expr ID) ID

trypaths :: [FilePath] -> IO String
trypaths [] = error "no path to open"
trypaths fs@(f:_) = foldr go (error $ "couldn't open" ++ f) fs
  where
    go fp z = catchJust err (readFile fp) (const z)
    err = guard . isDoesNotExistError

loadterms :: FilePath -> IO [Term]
loadterms path = do c <- trypaths [path, path <.> "sig"]
                    case runParser terms c of
                      [(ts,"")] -> return ts
                      _ -> error ("Syntax error in: " ++ path)

translate :: Bool -> [Term] -> IO ExprGroup
translate ceil = foldr incorp . pure $ Group [] [] [] ceil
  where
    inschild c g = return $ g { children = c : children g }
    insproms ps g = return $ g { promotes = ps ++ promotes g }
    insdef n d g = return $ g { defs = (n,d) : defs g }

    incorp t g' =
      do g <- g'
         case t of
           Inherit fp ps -> either (flip const) reimport ps
                              <$> loadprog fp >>= flip inschild g
           Bequeath ps -> insproms ps g
           TermGroup h -> translate False h >>= flip inschild g
           Define True p@(m,n) d -> insdef m d g >>= insproms [p]
           Define False (m,_) d -> insdef m d g
           Raw _ -> return g

reimport :: [(ID,ID)] -> ExprGroup -> ExprGroup
reimport ps g = g { promotes = foldr f [] ps }
  where
    xm = M.fromList . map swap $ promotes g
    f (m,n) qs = case M.lookup m xm of
                   Just l -> (l,n) : qs
                   Nothing -> error $ "Import error on: " ++ m

loadprog :: FilePath -> IO ExprGroup
loadprog p = let (d,f) = splitFileName p
             in withCurrentDirectory d $
                  loadterms p >>= translate True

loadstr :: String -> IO ExprGroup
loadstr s = case runParser terms s of
              [(ts,"")] -> translate True ts
              _ -> error "Syntax error"