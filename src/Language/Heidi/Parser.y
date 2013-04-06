{
-- Starting with example calculator from
-- http://www.haskell.org/happy/doc/html/sec-using.html#sec-other-datatypes
module Language.Heidi.Parser ( parse ) where

import Data.Char ( isSpace, isAlpha, isDigit )
import Language.Heidi.Lexer
}

%name parse
%tokentype { Token }
%error     { parseError }

%token
        var      { VarTok      }
        varName  { IdentTok $$ }
        real     { RealTok     }
        relation { RelationTok }
        int      { IntTok   $$ }

%%

-- should we return a token here?  why not for now? it has
-- everything we need, namely the int to be pulled out later

Int    : int     { IntTok $1 }

Exp    : var varName RealRelationDef { VarDec $2 $3 }
--       | varName                     { BareName $1 }

RealRelationDef : real relation { RelationDef }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data VarDec = VarDec String RelationDef
    deriving (Show)

data BareName = BareName String
    deriving (Show)

data RelationDef = RelationDef
    deriving (Show)

}
