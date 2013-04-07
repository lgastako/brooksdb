{

module Language.Heidi.Parser ( parse ) where

import Data.Char ( isSpace, isAlpha, isDigit )
import Language.Heidi.Lexer

}

%name parse
%tokentype { Token }
%error     { parseError }

%token
--      Keywords
        var             { VarTok           }
        init            { InitTok          }
        real            { RealTok          }
        base            { BaseTok          }
        relation        { RelationTok      }
        table_dee       { TableDeeTok      }
        table_dum       { TableDumTok      }
        same_type_as    { SameTypeAsTok    }
        same_heading_as { SameHeadingAsTok }
        with            { WithTok          }
        '('             { LeftRoundTok     }
        ')'             { RightRoundTok    }
        '{'             { LeftCurlyTok     }
        '}'             { RightCurlyTok    }
        '['             { LeftSquareTok    }
        ']'             { RightSquareTok   }
        ':'             { ColonTok         }

--      Parameterized tokens
        varName         { IdentTok $$      }
        int             { IntTok $$        }

%%

RealRelationVarDef : var RelationVarName RealOrBase RelationTypeOrInitValue KeyDefList { RealRelationVarDef }

--KeyDefList : ?

RealOrBase : real         { RealTok }
           | base         { BaseTok }

RelationVarName : varName                                           { RelationVarName $1 } -- guessed

RelationTypeOrInitValue : RelationTypeSpec                          { RelationTypeOrInitValueRelationTypeSpec $1 Nothing }
                        | init '(' RelationExp ')'                  { RelationTypeOrInitValueInit Nothing $3             }
                        | RelationTypeSpec init '(' RelationExp ')' { RelationTypeOrInitValueRelationTypeSpecInit $1 $4  }

RelationTypeSpec : RelationTypeName                                 { RelationTypeSpecRelationTypeName $1 }
                 | same_type_as '(' RelationExp ')'                 { RelationTypeSpecSameTypeAs $3       }
                 | relation same_heading_as '(' NonscalarExp ')'    { RelationTypeSpecSameHeadingAs $4    }

RelationTypeName : relation Heading                                 { RelationTypeName $2 }

Heading : '{' AttributeCommalist '}'                                { Heading $2 }

NonscalarExp : TupleExp                                             { NonScalarExpTupleExp $1 }
             | RelationExp                                          { NonScalarExpRelationExp $1 }

TupleExp : TupleWithExp                                             { $1 }
         | TupleNonwithExp                                          { $1 }

TupleWithExp : with '(' NameIntroCommalist ')' ':' TupleExp         { TupleWithExp $3 $6 }

TupleNonwithExp : TupleVarRef                                       { $1 }
                | TupleOpInv                                        { $1 }
                | ArrayVarRef '(' Subscript ')'                     { $1 }
                | '(' TupleExp ')'                                  { $1 }

Subscript : IntegerExp                                              { Subscript $1 }

IntegerExp : int                                                    { IntegerExpInt $1 }

RelationExp : RelationWithExp                                       { $1 }
            | RelationNonwithExp                                    { $1 }

RelationWithExp : with '(' NameIntroCommalist ')' ':' RelationExp   { RelationWithExp $3 $4 }  -- is the colon really right?

RelationNonwithExp : RelationVarRef                                 { RelationNonwithExpRelationVarRef $1 }
                   | RelationOpInv                                  { RelationNonwithExpRelationOpInv $1  }
                   | '(' RelationExp ')'                            { RelationNonwithExpRelationExp $2    }

RelationVarRef : RelationVarName                                    { RelationVarRef $1 }

RelationOpInv : UserOpInv                                           { $1 }
              | BuiltInRelationOpInv                                { $1 }

UserOpInv : UserOpName '(' ArgumentExpCommalist ')'                 { UserOpInv $1 $3 }

UserOpName : varName                                                { UserOpName $1 } -- guessed

-- ArgumentExpCommalist : ?

BuiltInRelationOpInv : RelationSelectorInv                          { $1 }
                     | THE_OpInv                                    { $1 }
                     | AttributeExtractorInv                        { $1 }
                     | Project                                      { $1 }
                     | NadicOtherBuiltInRelationOpInv               { $1 }
                     | MonadicOrDyadicOtherBuiltInRelationOpInv     { $1 }

THE_OpInv : THE_OpName '(' ScalarExp ')'                            { THE_OpInv $3 }

THE_OpName : varName                                                { THE_OpName $1 } -- guessed

ScalarExp : ScalarWithExp                                           { $1 }
          | ScalarNonwithExp                                        { $1 }

ScalarWithExp : with '(' NameIntroCommalist ')' ':' ScalarExp       { ScalarWithExp $3 $6 }

ScalarNonwithExp : ScalarVarRef                                     { $1 }
                 | ScalarOpInv                                      { $1 }
                 | '(' ScalarExp ')'                                { $1 }

ScalarVarRef : ScalarVarName                                        { ScalarVarRef $1 }

ScalarVarName : varName                                             { ScalarVarName $1 } -- guessed

RelationSelectorInv : relation '[' Heading ']' '{' TupleExpCommalist '}'   { RelationSelectorInv $1 $2 }
                    | table_dee                                            { RelationSelectorInvTableDee }
                    | table_dum                                            { RelationSelectorInvTableDum }



{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data VarDec = RelVarDec String RelationDef
            | IntVarDec String Int
            | BareName String
    deriving (Show)

data RelationDef = RelationDef
    deriving (Show)

}
