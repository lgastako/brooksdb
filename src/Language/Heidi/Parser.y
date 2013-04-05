{
-- Starting with example calculator from
-- http://www.haskell.org/happy/doc/html/sec-using.html#sec-other-datatypes
module Language.Heidi.Parser ( main ) where

import Data.Char ( isSpace, isAlpha, isDigit )
import Language.Heidi.Lexer
}

%name calc
%tokentype { Token }
%error     { parseError }

%token
        var      { VarTok      }
        varName  { IdentTok $$ }
        real     { RealTok     }
        relation { RelationTok }
        int      { IntTok   $$ }

%%

Exp : var varName valDef { VarDec $2 $3 }

valDef : real relation { RelationDef }




{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data VarDec = VarDec String RelationDef
    deriving (Show)

data RelationDef = RelationDef
    deriving (Show)

--main = getContents >>= print . calc . alexScanTokens

main = return "var x real relation" >>= print . calc . alexScanTokens
}
