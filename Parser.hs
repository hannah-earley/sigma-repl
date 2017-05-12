{-# LANGUAGE TupleSections #-}

module Parser
( Term(..)
, SigmaToken(..)
, ParseResult(..)
, term
, stok
, parseResult
) where

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Error as E
import Text.Parsec ((<|>), (<?>))
import Text.Printf
import Data.Char (isSpace, isDigit, isControl)
import Data.Functor.Identity (Identity)
import Control.Monad (guard)
import Numeric.Natural (Natural)

--- parsec bootstrapping

lexer :: T.GenTokenParser String () Identity
lexer = T.makeTokenParser T.LanguageDef
      { T.commentStart = "(*"
      , T.commentEnd = "*)"
      , T.commentLine = ";"
      , T.nestedComments = True
      , T.identStart = P.satisfy $ not . reservedIdStart
      , T.identLetter = P.satisfy $ not . reservedIdLetter
      , T.reservedNames = [ "inh", "inh*", "beq", "beq*", "grp"
                          , "def", "def*", "perm", "perm*" ]
      , T.caseSensitive = True
      , T.opStart = undefined
      , T.opLetter = undefined
      , T.reservedOpNames = undefined }
  where
    nonVisible c = isControl c || isSpace c
    reservedIdLetter c = nonVisible c || c `elem` ".#@<>(){}[]`':\""
    reservedIdStart c = reservedIdLetter c || isDigit c

symbol = T.symbol lexer
identifier = T.identifier lexer
reserved = T.reserved lexer
litStr = T.stringLiteral lexer
litNat = T.natural lexer
parens = T.parens lexer
braces = T.braces lexer
angles = T.angles lexer
brackets = T.brackets lexer
colon = T.colon lexer
dot = T.dot lexer

--- miscellaneous

filepath = litStr <?> "file path"

-- match many p followed by one q, where p and q may intersect
-- should match p*, then try q and backtrack if it matched one too many ps
manybut p q = P.try go <|> ([],) <$> q
  where
    go = mapFst . (:) <$> p >>= (<$> manybut p q)
    mapFst f (a,b) = (f a, b)

data ParseResult a = ParseOK a
                   | ParseError String
                   | ParseIncomplete String
                   deriving (Show)

-- SysUnExpect "" corresponds to an unexpected eof, which is useful
-- for multiline repl inputs, otherwise we just extract the parsec error
-- as a string if it exists else return the result
parseResult :: Either P.ParseError a -> ParseResult a
parseResult (Right r) = ParseOK r
parseResult (Left e)
  | incompletep e = ParseIncomplete $ show e
  | otherwise = ParseError $ show e

-- note that SysUnExpect m1 == SysUnExpect m2 \forall m1,m2
-- so we have to first filter unexpects, then check for "" membership
incompletep = elem "" . map E.messageString
                      . filter (E.SysUnExpect "" ==)
                      . E.errorMessages

--- program expressions

data Term = InheritAll FilePath String
          | InheritSome FilePath [(String,String)]
          | BequeathAll
          | BequeathSome [(String,String)]
          | Group [Term]
          | LocalDef String SigmaToken
          | BequeathDef (String,String) SigmaToken
          deriving (Show)

term = P.labels (parens $ tinha <|> tinhs <|> tbeqa <|> tbeqs <|> tgrp
                                <|> tdefb <|> tdefl <|> tpermb <|> tperml)
         ["inheritance", "bequest", "group", "definition"]

tinhs = reserved "inh" >> InheritSome <$> filepath >>= (<$> P.many defid)
tinha = reserved "inh*" >> InheritAll <$> filepath
                        >>= (<$> P.option "" identifier)

tbeqs = reserved "beq" >> BequeathSome <$> P.many defid
tbeqa = reserved "beq*" >> return BequeathAll

tgrp = reserved "grp" >> Group <$> P.many term

tdefl = reserved "def" >> LocalDef <$> identifier >>= (<$> stok)
tdefb = reserved "def*" >> BequeathDef <$> defid >>= (<$> stok)

tperml = do reserved "perm"
            (n,l) <- parens plhs
            (r,n') <- parens prhs
            pval (n,n) n'
            return . LocalDef n $ SigmaPerm l r
tpermb = do reserved "perm*"
            (n,l) <- parens plhs'
            (r,n') <- parens prhs
            pval n n'
            return . BequeathDef n $ SigmaPerm l r

--- sigma expressions

-- SigmaList and SigmaNat are for sugared structures which require
-- cons,nil,succ,zero to be defined before they can be desugared
data SigmaToken = SigmaSeq [SigmaToken]
                | SigmaLabel String
                | SigmaRef String
                | SigmaPerm [SigmaToken] [SigmaToken]
                | SigmaList [SigmaToken] (Maybe SigmaToken)
                | SigmaNat Natural
                deriving (Show)
-- - in top level sequences, labels become refs
-- - we could add support for as-expressions in future,
--   though this necessitates a consistency check...

defid' f = (symbol "@" >> identifier) <|> return f
defid = do a <- identifier
           b <- defid' a
           return (a,b)

stok = sseq <|> slab <|> sref <|> sperm <|> ssugar
sseq = SigmaSeq <$> parens (P.many stok) <?> "sequence"
slab = SigmaLabel <$> identifier <?> "label"
sref = C.char '`' >> SigmaRef <$> identifier <?> "reference"
sperm = snd <$> angles perm <?> "permutation"

-- sigma perms

plhs = (,) <$> identifier >>= (<$> P.many stok)
plhs' = (,) <$> defid >>= (<$> P.many stok)
prhs = manybut stok identifier

pval (a,b) c
  | a == b = guard (a == c) <?> printf "bottom identifier of '%s'" a
  | otherwise = guard (c == a || c == b)
                <?> printf "bottom identifier of '%s' or '%s'" a b

perm = do (n,l) <- plhs
          colon
          (r,n') <- prhs
          pval (n,n) n'
          return (n, SigmaPerm l r)

--- sigma sugar

ssugar = sstop <|> sdata <|> snat <|> slist

stop = SigmaSeq []
wrap xs = SigmaSeq $ [stop] ++ xs ++ [stop]

sstop = symbol "#" >> return stop <?> "stop"
sdata = wrap <$> braces (P.many stok) <?> "data sequence"
snat = SigmaNat . fromInteger <$> litNat <?> "natural number"
slist = brackets (P.optionMaybe stok >>= slist') <?> "list"

slist' Nothing = return $ SigmaList [] Nothing
slist' (Just x) = SigmaList . (x:) <$> P.many stok
                     >>= (<$> P.optionMaybe (dot >> stok))
