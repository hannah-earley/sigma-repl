module Input
( module Input
) where

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as T
import Data.Char (isSpace, isDigit)
import Data.Functor.Identity (Identity)


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
    reservedIdLetter c = isSpace c || c `elem` ".#@<>(){}[]`':\""
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
