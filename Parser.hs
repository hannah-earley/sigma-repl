{-# LANGUAGE TupleSections #-}

module Parser
( Term(..)
, SigmaToken(..)
, SigmaRef'(..)
, ParseResult(..)
, term
, terms
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
import System.FilePath.Posix (FilePath)

--- parsec bootstrapping

type Parser = P.Parsec String ()

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
    reservedIdLetter c = nonVisible c || c `elem` ".#@<>(){}[]`:\""
    reservedIdStart c = reservedIdLetter c || isDigit c

symbol = T.symbol lexer         -- String -> Parser String
identifier = T.identifier lexer -- Parser String
reserved = T.reserved lexer     -- String -> Parser ()
litStr = T.stringLiteral lexer  -- Parser String
litNat = T.natural lexer        -- Parser Integer
parens = T.parens lexer         -- Parser a -> Parser a
braces = T.braces lexer         -- Parser a -> Parser a
angles = T.angles lexer         -- Parser a -> Parser a
brackets = T.brackets lexer     -- Parser a -> Parser a
colon = T.colon lexer           -- Parser String
dot = T.dot lexer               -- Parser String

--- result interpretation

data ParseResult a = ParseOK a
                   | ParseError String
                   | ParseIncomplete String
                   deriving (Show)

parseResult :: Either P.ParseError a -> ParseResult a
parseResult (Right r) = ParseOK r
parseResult (Left e) = incompletep e $ show e

incompletep :: P.ParseError -> String -> ParseResult a
incompletep = go . E.errorMessages
  where go (E.SysUnExpect "" : _) = ParseIncomplete
        go (_ : xs) = go xs
        go [] = ParseError

--- program expressions

data Term = InheritAll FilePath String
          | InheritSome FilePath [(String,String)]
          | BequeathAll
          | BequeathSome [(String,String)]
          | Group [Term]
          | LocalDef String SigmaToken
          | BequeathDef (String,String) SigmaToken
          deriving (Show)

terms :: Parser [Term]
terms = P.manyTill term P.eof

term :: Parser Term
term = P.labels (parens ttok) labels
  where
    labels = ["inheritance", "bequest", "group", "definition"]
    ttok = tinha <|> tinhs <|> tbeqa <|> tbeqs <|> tgrp
                 <|> tdefb <|> tdefl <|> tpermb <|> tperml

    filepath = litStr <?> "file path"
    tinhs = reserved "inh" >> InheritSome <$> filepath >>= (<$> P.many defid)
    tinha = reserved "inh*" >> InheritAll <$> filepath
                            >>= (<$> P.option "" identifier)

    tbeqs = reserved "beq" >> BequeathSome <$> P.many defid
    tbeqa = reserved "beq*" >> return BequeathAll

    tgrp = reserved "grp" >> Group <$> P.many term

    tdefl = reserved "def" >> LocalDef <$> identifier >>= (<$> stok)
    tdefb = reserved "def*" >> BequeathDef <$> defid >>= (<$> stok)

    tperml = do reserved "perm"
                (n,l) <- parens $ plhs identifier
                (r,n') <- parens prhs
                pval (n,n) n'
                return . LocalDef n $ SigmaPerm l r

    tpermb = do reserved "perm*"
                (n,l) <- parens $ plhs defid
                (r,n') <- parens prhs
                pval n n'
                return . BequeathDef n $ SigmaPerm l r

--- sigma expressions

data SigmaToken = SigmaSeq [SigmaToken]
                | SigmaLabel String
                | SigmaRef String
                | SigmaRef' SigmaRef'
                | SigmaPerm [SigmaToken] [SigmaToken]
                deriving (Show)

stok :: Parser SigmaToken
stok = sseq <|> slab <|> sref <|> sperm <|> ssugar
  where
    sseq = SigmaSeq <$> parens (P.many stok) <?> "sequence"
    slab = SigmaLabel <$> identifier <?> "label"
    sref = C.char '`' >> SigmaRef <$> identifier <?> "reference"
    sperm = snd <$> angles perm <?> "permutation"

    perm = do (n,l) <- plhs identifier
              colon
              (r,n') <- prhs
              pval (n,n) n'
              return (n, SigmaPerm l r)

--- sigma sugar

data SigmaRef' = SCons | SNil | SSucc | SZero deriving (Show)

ssugar :: Parser SigmaToken
ssugar = sstop <|> sdata <|> snat <|> slist
  where
    stop = SigmaSeq []
    wrap xs = SigmaSeq $ [stop] ++ xs ++ [stop]

    sstop = symbol "#" >> return stop <?> "stop"

    sdata = wrap <$> braces (P.many stok) <?> "data sequence"

    snat = wnat <$> litNat <?> "natural number"
      where
        wnat 0 = wrap [SigmaRef' SZero, stop]
        wnat n = wrap [SigmaRef' SSucc, wnat (n-1)]

    slist = brackets (P.optionMaybe stok >>= slist') <?> "list"
      where
        co x z = wrap [SigmaRef' SCons, wrap [x,z]]
        ni = wrap [SigmaRef' SNil, stop]
        slist' Nothing = return ni
        slist' (Just x) = flip (foldr co) . (x:) <$> P.many stok
                            >>= (<$> P.option ni (dot >> stok))

-- sigma helpers

manybut :: Parser a -> Parser b -> Parser ([a],b)
manybut p q = P.try go <|> ([],) <$> q
  where
    go = mapFst . (:) <$> p >>= (<$> manybut p q)
    mapFst f (a,b) = (f a, b)

defid :: Parser (String,String)
defid = do a <- identifier
           b <- P.option a (symbol "@" >> identifier)
           return (a,b)

plhs :: Parser n -> Parser (n,[SigmaToken])
plhs p = (,) <$> p >>= (<$> P.many stok)

prhs :: Parser ([SigmaToken],String)
prhs = manybut stok identifier

pval :: (String,String) -> String -> Parser ()
pval (a,b) c
  | a == b = guard (a == c) <?> printf "bottom identifier of '%s'" a
  | otherwise = guard (c == a || c == b)
                <?> printf "bottom identifier of '%s' or '%s'" a b
