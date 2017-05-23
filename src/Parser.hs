{-# LANGUAGE TupleSections #-}

module Parser
( Term(..)
, SigmaToken(..)
, ParseResult(..)
, term
, terms
, stok
, parseResult
, cmd
) where

import Common (ExtendedNat(..))
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
ws = T.whiteSpace lexer         -- Parser ()

--- result interpretation

data ParseResult a = ParseOK a
                   | ParseError String
                   | ParseIncomplete String
                   deriving (Show)

parseResult :: Either P.ParseError a -> ParseResult a
parseResult (Right r) = ParseOK r
parseResult (Left e) = incompletep e $ show e

incompletep :: P.ParseError -> String -> ParseResult a
incompletep = fin . foldl go (False,True) . E.errorMessages
  where
    go (_,b) (E.SysUnExpect "") = (True,b)
    go (a,_) (E.UnExpect _) = (a,False)
    go ab _ = ab

    fin (True,True) = ParseIncomplete
    fin _ = ParseError

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
terms = ws >> P.manyTill term P.eof

terms1 :: Parser [Term]
terms1 = ws >> (:) <$> term >>= (<$> terms)

term :: Parser Term
term = P.labels (parens ttok) labels
  where
    labels = ["inheritance", "bequest", "group", "definition"]
    ttok = tinha <|> tinhs <|> tbeqa <|> tbeqs <|> tgrp
                 <|> tdefb <|> tdefl <|> tpermb <|> tperml

    filepath = litStr <?> "file path"
    tinhs = reserved "inh" >> InheritSome <$> filepath
                           >>= (<$> P.many defid)
    tinha = reserved "inh*" >> InheritAll <$> filepath
                            >>= (<$> P.option "" identifier)

    tbeqs = reserved "beq" >> BequeathSome <$> P.many defid
    tbeqa = reserved "beq*" >> return BequeathAll

    tgrp = reserved "grp" >> Group <$> P.many term

    tdefl = reserved "def" >> LocalDef <$> identifier >>= (<$> stok)
    tdefb = reserved "def*" >> BequeathDef <$> defid >>= (<$> stok)

    tperml = do reserved "perm"
                (n,l) <- parens $ plhs identifier
                (r,n') <- parens $ prhs identifier
                pval (n,n) n'
                return . LocalDef n $ mkperm n l r

    tpermb = do reserved "perm*"
                (n,l) <- parens $ plhs defid
                (r,n') <- parens $ prhs identifier
                pval n n'
                return . BequeathDef n $ mkperm (fst n) l r

--- sigma expressions

data SigmaToken = SigmaSeq [SigmaToken]
                | SigmaLabel String
                | SigmaRef String
                | SigmaRef' String
                | SigmaPerm [SigmaToken] [SigmaToken]
                deriving (Show)

mkperm :: String -> [SigmaToken] -> [SigmaToken] -> SigmaToken
mkperm n l r = SigmaPerm ([SigmaLabel n] ++ l) (r ++ [SigmaLabel n])

stok :: Parser SigmaToken
stok = sseq <|> slab <|> sref <|> sperm <|> ssugar
  where
    sseq = SigmaSeq <$> parens (P.many stok) <?> "sequence"
    slab = SigmaLabel <$> identifier <?> "label"
    sref = C.char '`' >> SigmaRef <$> identifier <?> "reference"
    sperm = angles perm <?> "permutation"

    perm = do (n,l) <- plhs identifier
              colon
              (r,n') <- prhs identifier
              pval (n,n) n'
              return $ mkperm n l r

--- sigma sugar

ssugar :: Parser SigmaToken
ssugar = sstop <|> sdata <|> snat <|> slist
  where
    stop = SigmaSeq []
    wrap xs = SigmaSeq $ [stop] ++ xs ++ [stop]

    sstop = symbol "#" >> return stop <?> "stop"

    sdata = wrap <$> braces (P.many stok) <?> "data sequence"

    snat = wnat <$> litNat <?> "natural number"
      where
        wnat 0 = wrap [SigmaRef' "zero", stop]
        wnat n = wrap [SigmaRef' "succ", wnat (n-1)]

    slist = brackets (P.optionMaybe stok >>= slist') <?> "list"
      where
        co x z = wrap [SigmaRef' "cons", x, z]
        ni = wrap [SigmaRef' "nil", stop, stop]
        slist' Nothing = return ni
        slist' (Just x) = flip (foldr co) . (x:) <$> P.many stok
                            >>= (<$> P.option ni (dot >> stok))

--- sigma helpers

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

prhs :: Parser n -> Parser ([SigmaToken],n)
prhs p = manybut stok p

pval :: (String,String) -> String -> Parser ()
pval (a,b) c
  | a == b = guard (a == c) <?> printf "bottom identifier of '%s'" a
  | otherwise = guard (c == a || c == b)
                <?> printf "bottom identifier of '%s' or '%s'" a b

--- commands

data EvalMode = Manual | Automatic deriving (Show)

data Command = LoadFile FilePath
             | LoadRaw String
             | Reload
             | Eval EvalMode SigmaToken
             | Relimit ExtendedNat
             | Quit
             | Noop
             deriving (Show)

cmd :: Parser Command
cmd = C.spaces >> (meta <|> P.try eauto <|> lraw <|> noop)
  where
    meta = C.char ':'
              >> (lre <|> lfile <|> lrawmulti <|> eman
                      <|> relim <|> unlim <|> quit)
              <?> "meta command"

    lre = C.char 'r' >> return Reload <?> "reload (:r)"
    lfile = C.char 'l' >> P.many1 C.space
              >> LoadFile <$> P.many1 C.anyChar
              <?> "file to load (:l path)"
    lraw = P.lookAhead terms1 >> LoadRaw <$> P.many C.anyChar
              <?> "definition set"
    lrawmulti = do { C.char '{'
                   ; P.lookAhead (ws >> P.manyTill term eog)
                   ; LoadRaw <$> P.manyTill C.anyChar (P.try eog)
                   } <?> "multiline definition set (:{ ... })"
      where eog = C.char '}' >> C.spaces >> P.eof

    eauto = do { s <- stok ; P.eof ; return $ Eval Automatic s }
              <?> "sigma expression"
    eman = C.char 'e' >> ws >> Eval Manual <$> stok
              <?> "manual evaluation (:e sigma)"

    unlim = C.char 'u' >> return (Relimit Infinite) <?> "unlimit (:u)"
    relim = C.char 'n' >> C.spaces
              >> Relimit . Finite . fromIntegral <$> litNat
              <?> "relimit (:n nat)"

    quit = C.char 'q' >> return Quit <?> "quit (:q)"
    noop = P.eof >> return Noop
