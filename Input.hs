{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ViewPatterns #-}

module Input
( loadprog
, loadstr
, loadexpr
, runcmd
, Command(..)
, EvalMode(..)
, InterpreterException(..)
) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Numeric.Natural
import Data.Tuple

import Control.Applicative
import Control.Monad
import Control.Exception (Exception, throwIO, displayException, handle)
import Data.Typeable (Typeable)

import System.IO (readFile)
import System.IO.Error (isDoesNotExistError)
import System.Directory (withCurrentDirectory)
import System.FilePath.Posix (splitFileName, (<.>))
import Control.Exception (catchJust)

import Model (Expr(..))
import Module (Group(..))

---

data InterpreterException = LocateError FilePath
                          | SyntaxError FilePath
                          | ImportError FilePath ID
                          | IncompleteParseError
                          | OtherError String
                          deriving (Show, Typeable)

instance Exception InterpreterException where
  displayException (LocateError p) = "Couldn't locate: " ++ p
  displayException (SyntaxError p) = "Syntax error in: " ++ p
  displayException (ImportError p s) = "In: " ++ p ++ "; Couldn't import: " ++ s
  displayException (IncompleteParseError) = "Unexpected end of input"
  displayException (OtherError s) = s

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
                            (x:_) -> [x]

eof :: Parser ()
eof = Parser $ \inp -> case inp of
                         "" -> [((),"")]
                         _ -> []

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

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

-- lower = sat $ between 'a' 'z'
-- upper = sat $ between 'A' 'Z'
digit = sat $ between '0' '9'
-- alpha = lower <|> upper
-- alphanum = alpha <|> digit

chars :: [Char] -> Parser Char
chars cs = foldl (<|>) mzero $ map char cs

string :: String -> Parser String
string ""     = return ""
string (x:xs) = (:) <$> char x <*> string xs

--- brackets

bracket :: Parser a -> Parser b -> Parser c -> Parser c
bracket l r p = l *> p <* r

--- lexing

ignore :: Parser a -> Parser ()
ignore = (const () <$>)

spaces :: Parser ()
spaces = ignore . many1 $ chars " \t\n\f\r\v"

comment :: Parser ()
comment = smallest . ignore
                   $ bracket (string "(*") (string "*)")
                             (many $ comment <||> ignore item)

junk :: Parser ()
junk = ignore . many $ spaces <||> comment

parse :: Parser a -> Parser a
parse = (junk *>)

token :: Parser a -> Parser a
token = (<* junk)

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
strlit c = brack [c,c] . many $ stresc <||> sat (/= c)

litstr :: Char -> Parser String
litstr = token . strlit

canonstr :: Parser String
canonstr = litstr '"' <||> litstr '\''

--- natural number literal

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

paren = bracket (symbol "(") (symbol ")")
brack (l:r:"") = bracket (symbol [l]) (symbol [r])

--- sigma expressions

type Expr' = Expr ID ID

expr :: Parser Expr'
expr = largest $ parse atom
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

    lit = lnat <||> ldata <||> llist <||> llistimp

    ldata = brack "{}" $ wrap <$> many expr

    llistimp = brack "[]" $ do cars <- many1 expr
                               symbol ":"
                               cdr <- expr
                               return $ foldr co cdr cars

    llist = brack "[]" $ foldr co ni <$> many expr

    co x z = wrap [Ref "cons", wrap [x, z]]
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

terms :: Parser [Term]
terms = largest . parse $ many term
  where

    term = largest atom

    atom = inherit <||> bequeath <||> group <||> def <||> raw

    inherit = all' <||> some'
      where
        all' = env "inherit*" $ flip Inherit (Left ()) <$> canonstr
        some' = env "inherit" $
                liftM2 Inherit canonstr (Right <$> many promoter)

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

    promoter = labelgen (\l -> (l, l)) (,) variable

    env n p = brack "()" $ symbol n >> p

--- program translation

type ExprGroup = Group (Expr ID) ID

trypaths :: [FilePath] -> IO String
trypaths [] = error "trypaths: no paths to try"
trypaths fs@(f:_) = foldr go (throwIO $ LocateError f) fs
  where
    go fp z = catchJust err (readFile fp) (const z)
    err = guard . isDoesNotExistError

loadterms :: FilePath -> IO [Term]
loadterms path = do c <- trypaths [path, path <.> "sig"]
                    case runParser terms c of
                      [(ts,"")] -> return ts
                      _ -> throwIO $ SyntaxError path

-- translate a list of terms to a term group
-- bool argument specifies whether this is a top-level module
translate :: Bool -> [Term] -> IO ExprGroup
translate = foldr incorp . pure . Group [] [] []
  where
    inschild = liftM2 $ \c g -> g { children = c : children g }
    insproms = \ps g -> g { promotes = ps ++ promotes g }
    insdef = \nd g -> g { defs = nd : defs g }

    incorp (Inherit fp ps) = inschild . either (flip const) ((=<<) . reimport) ps $ loadprog fp
    incorp (Bequeath ps) = (insproms ps <$>)
    incorp (TermGroup h) = inschild $ translate False h
    incorp (Define b p@(m,_) d) = (insproms (if b then [p] else []) <$> insdef (m,d) <$>)
    incorp _ = id

reimport :: [(ID,ID)] -> ExprGroup -> IO ExprGroup
reimport ps g = do ps' <- foldM f [] ps
                   return g { promotes = ps' }
  where
    xm = M.fromList . map swap $ promotes g
    f qs (m,n) = case M.lookup m xm of
                   Just l -> return $ (l,n) : qs
                   Nothing -> throwIO $ ImportError "" (m ++ " <> " ++ show g)

loadprog :: FilePath -> IO ExprGroup
loadprog p = let (d,f) = splitFileName p
             in withCurrentDirectory d . imperrloc f $
                  loadterms f >>= translate True

imperrloc :: FilePath -> IO ExprGroup -> IO ExprGroup
imperrloc p = handle (throwIO . go)
  where go (ImportError "" m) = ImportError p m
        go e = e

loadstr :: String -> IO ExprGroup
loadstr s = let p = "(input)" in case runParser terms s of
              [(ts,"")] -> imperrloc p $ translate True ts
              _ -> throwIO $ SyntaxError p

loadexpr :: String -> Maybe (Expr ID ID)
loadexpr s = case runParser expr s of
               [(e,"")] -> Just e
               _ -> Nothing

--- metacommands

data EvalMode = Manual | Automatic

data Command = Load ExprGroup
             | Eval EvalMode (Expr ID ID)
             | Relimit (Either () (Maybe Int))
             | Quit
             | Info
             | Noop

cmd :: Parser (IO Command)
cmd = parse $ largest option
  where
    option = noop <||> quit <||> info <||> load <||> group <||> limit <||> eval <||> run

    noop = pure Noop <$ parse eof

    quit = (symbol ":q" <||> symbol "quit" <||> symbol "exit") >> return (pure Quit)

    info = symbol ":i" >> return (pure Info)

    load = symbol ":l" >> (Load <$>) <$> loadprog <$> canonstr <||> rest

    group1 = symbol ":{" *> ((Load <$>) <$> loadstr <$> rest) <* symbol ":}"
    group2 = symbol ":{" >> rest >> (pure . throwIO $ IncompleteParseError)
    group = group1 <||> group2

    limit = symbol ":n" >> return . Relimit <$> limitn <||> limitq
      where
        limitn = Right . limit' <$> nat
        limitq = pure $ Left ()
        limit' 0 = Nothing
        limit' n = Just (fromIntegral n)

    eval = symbol ":e" >> let err = throwIO $ SyntaxError "(input)"
                          in (Eval Manual <$>) <$> maybe err return
                                                . loadexpr <$> rest

    run = do e <- rest
             pure $ case loadexpr e of
                      Nothing -> Load <$> loadstr e
                      Just e' -> return $ Eval Automatic e'

    rest = token $ many item


runcmd :: String -> IO (Either InterpreterException Command)
runcmd (runParser cmd -> [(c,"")]) = handle (return . Left) $ Right <$> c
runcmd _ = return . Left $ OtherError "Bad command"