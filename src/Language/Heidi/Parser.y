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
        tuple           { TupleTok         }
        from            { FromTok          }
        all             { AllTok           }
        but             { ButTok           }
        union           { UnionTok         }
        rename          { RenameTok        }
        extend          { ExtendTok        }
        wrap            { WrapTok          }
        unwrap          { UnwrapTok        }
        as              { AsTok            }
        compose         { ComposeTok       }
        heading         { HeadingTok       }
        d_union         { DunionTok        }
        intersect       { IntersectTok     }
        join            { JoinTok          }
        times           { TimesTok         }
        xunion          { XunionTok        }
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

RealOrBase : real                                                   { RealTok }
           | base                                                   { BaseTok }

RelationVarName : varName                                           { RelationVarName $1 }

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

ArrayVarRef : ArrayVarName                                          { ArrayVarRef $1 }

ArrayVarName : varName                                              { ArrayVarName $1 }

TupleVarRef : TupleVarName                                          { TupleVarRef $1 }

TupleVarName : varName                                              { TupleVarName $1 }

TupleOpInv : UserOpInv                                              { TupleOpInvUser $1 }
           | BuiltInTupleOpInv                                      { TupleOpInvBuiltIn $1 }

BuiltInTupleOpInv : TupleSelectorInv                                { BuiltInTupleOpInv $1 }
                  | THE_OpInv                                       { BuiltInTupleOpInv $1 }
                  | AttributeExtractorInv                           { BuiltInTupleOpInv $1 }
                  | TupleExtractorInv                               { BuiltInTupleOpInv $1 }
                  | TupleProject                                    { BuiltInTupleOpInv $1 }
                  | NadicOtherBuiltInTupleOpInv                     { BuiltInTupleOpInv $1 }
                  | MonadicOrDyadicOtherBuiltInTupleOpInv           { BuiltInTupleOpInv $1 }

TupleSelectorInv : tuple '{' TupleComponentCommalist '}'            { TupleSelectorInv $3 }

AttributeExtractorInv : AttributeRef from TupleExp                  { AttributeExtractorInv $1 $3 }

AttributeRef : AttributeName                                        { AttributeRef $1 }

AttributeName : varName                                             { AttributeName $1 }

TupleExtractorInv : tuple from RelationExp                          { TupleExtractorInv $3 }

TupleProject : TupleExp '{' AttributeRefCommalist '}'               { TupleProject $1 $3 }
             | TupleExp '{' all but AttributeRefCommalist '}'       { TupleProjectAllBut $1 $5 }

NadicOtherBuiltInTupleOpInv : NadicTupleUnion                       { NadicOtherBuiltInTupleOpInv $1 }

NadicTupleUnion : union '{' TupleExpCommalist '}'                   { NadicTupleUnion $3 }

MonadicOrDyadicOtherBuiltInTupleOpInv : MonadicOtherBuiltInTupleOpInv { MonadicOrDyadicOtherBuiltInTupleOpInv $1 }
                                      | DyadicOtherBuiltInTupleOpInv  { MonadicOrDyadicOtherBuiltInTupleOpInv $1 }

MonadicOtherBuiltInTupleOpInv : TupleRename                         { MonadicOtherBuiltInTupleOpInv $1 }
                              | TupleExtend                         { MonadicOtherBuiltInTupleOpInv $1 }
                              | TupleWrap                           { MonadicOtherBuiltInTupleOpInv $1 }
                              | TupleUnwrap                         { MonadicOtherBuiltInTupleOpInv $1 }

DyadicOtherBuiltInTupleOpInv : DyadicTupleUnion                     { DyadicOtherBuiltInTupleOpInvUnion $1 }
                             | DyadicTupleCompose                   { DyadicOtherBuiltInTupleOpInvCompose $1 }

DyadicTupleUnion : TupleExp union TupleExp                          { DyadicTupleUnion $1 $3 }

DyadicTupleCompose : TupleExp compose TupleExp                      { DyadicTupleCompose $1 $3 }

TupleRename : TupleExp rename '{' RenamingCommalist '}'             { TupleRename $2 $4 }

TupleExtend : extend TupleExp ':' '{' AttributeAssignCommalist '}'  { TupleExtend $2 $5 }

TupleWrap : TupleExp wrap '(' Wrapping ')'                          { TupleWrap $1 $4 }

TupleUnwrap : TupleExp unwrap '(' Unwrapping ')'                    { TupleUnwrap $1 $4 }

Wrapping : '{' AttributeRefCommalist '}' as IntroducedName          { Wrapping $2 $5 }
         | '{' all but AttributeRefCommalist '}' as IntroducedName  { WrappingAllBut $4 $7 }

IntroducedName : varName                                            { IntroducedName $1 }

Unwrapping : AttributeRef                                           { Unwrapping $1 }

-- AttributeRefCommalist : ?

Subscript : IntegerExp                                              { Subscript $1 }

IntegerExp : int                                                    { IntegerExpInt $1 }

RelationExp : RelationWithExp                                       { RelationExpWith $1 }
            | RelationNonwithExp                                    { RelationExpNonwith $1 }

RelationWithExp : with '(' NameIntroCommalist ')' ':' RelationExp   { RelationWithExp $3 $4 }

RelationNonwithExp : RelationVarRef                                 { RelationNonwithExpRelationVarRef $1 }
                   | RelationOpInv                                  { RelationNonwithExpRelationOpInv $1  }
                   | '(' RelationExp ')'                            { RelationNonwithExpRelationExp $2    }

RelationVarRef : RelationVarName                                    { RelationVarRef $1 }

RelationOpInv : UserOpInv                                           { RelationOpInv $1 }
              | BuiltInRelationOpInv                                { RelationOpInv $1 }

UserOpInv : UserOpName '(' ArgumentExpCommalist ')'                 { UserOpInv $1 $3 }

UserOpName : varName                                                { UserOpName $1 }

-- ArgumentExpCommalist : ?

BuiltInRelationOpInv : RelationSelectorInv                          { BuiltInRelationOpInvRelationSelectorInv $1                      }
                     | THE_OpInv                                    { BuiltInRelationOpInvTHE_OpInv $1                                }
                     | AttributeExtractorInv                        { BuiltInRelationOpInvAttributeExtractorInv $1                    }
                     | Project                                      { BuiltInRelationOpInvProject $1                                  }
                     | NadicOtherBuiltInRelationOpInv               { BuiltInRelationOpInvNadicOtherBuiltInRelationOpInv $1           }
                     | MonadicOrDyadicOtherBuiltInRelationOpInv     { BuiltInRelationOpInvMonadicOrDyadicOtherBuiltInRelationOpInv $1 }

THE_OpInv : THE_OpName '(' ScalarExp ')'                            { THE_OpInv $3 }

THE_OpName : varName                                                { THE_OpName $1 }

NadicOtherBuiltInRelationOpInv : NadicUnion                         { NadicOtherBuiltInRelationOpInv $1 }
                               | NadicDisjointUnion                 { NadicOtherBuiltInRelationOpInv $1 }
                               | NadicIntersect                     { NadicOtherBuiltInRelationOpInv $1 }
                               | NadicJoin                          { NadicOtherBuiltInRelationOpInv $1 }
                               | NadicTimes                         { NadicOtherBuiltInRelationOpInv $1 }
                               | NadicXunion                        { NadicOtherBuiltInRelationOpInv $1 }
                               | NadicCompose                       { NadicOtherBuiltInRelationOpInv $1 }

NadicUnion : union '{' RelationExpCommalist '}'                     { NadicUnion $1          }
           | union heading '{' RelationExpCommalist '}'             { NadicUnionHeaded $1 $2 }

NadicDisjointUnion : d_union '{' RelationExpCommalist '}'           { NadicDisjointUnion $1 }
                   | d_union heading '{' RelationExpCommalist '}'   { NadicDisjointUnionHeaded $1 $2 }

NadicIntersect : intersect '{' RelationExpCommalist '}'             { NadicIntersect $1          }
               | intersect heading '{' RelationExpCommalist '}'     { NadicIntersectHeaded $1 $2 }

NadicJoin : join '{' RelationExpCommalist '}'                       { NadicJoin $3 }

NadicTimes : join '{' RelationExpCommalist '}'                      { NadicTimes $3 }

NadicXunion : xunion '{' RelationExpCommalist '}'                   { NadicXunion $1          }
            | xunion heading '{' RelationExpCommalist '}'           { NadicXunionHeaded $1 $2 }

NadicCompose : join '{' RelationExpCommalist '}'                    { NadicCompose $3 }

ScalarExp : ScalarWithExp                                           { SclarExpScalarWithExp $1    }
          | ScalarNonwithExp                                        { SclarExpScalarNonwithExp $1 }

ScalarWithExp : with '(' NameIntroCommalist ')' ':' ScalarExp       { ScalarWithExp $3 $6 }

ScalarNonwithExp : ScalarVarRef                                     { ScalarNonwithExpScalarVarRef $1 }
                 | ScalarOpInv                                      { ScalarNonwithExpScalarOpInv $1  }
                 | '(' ScalarExp ')'                                { ScalarNonwithExpScalarExp $1    }

ScalarVarRef : ScalarVarName                                        { ScalarVarRef $1 }

ScalarVarName : varName                                             { ScalarVarName $1 }

ScalarOpInv : UserOpInv                                             { ScalarOpInv $1 }
            | BuiltInScalarOpInv                                    { ScalarOpInv $1 }

BuiltInScalarOpInv : ScalarSelectorInv                              { BuiltInScalarOpInv $1 }
                   | THE_OpInv                                      { BuiltInScalarOpInv $1 }
                   | AttributeExtractorInv                          { BuiltInScalarOpInv $1 }
                   --| AggOpInv                                       { BuiltInScalarOpInv $1 }
                   -- "plus the usual possibilities...eh?"

ScalarSelectorInv : BuiltInScalarLiteral                            { ScalarSelectorInv $1 }
                  | PossrepName '(' ArgumentExpCommalist ')'        { ScalarSelectorInv $1 $3 }

-- BuiltInScalarLiteral : ?

PossrepName : varName                                               { PossrepName $1 }

--AggOpInv : AggOpName '(' RelationExp ')'
           --AggOpName '(' IntegerExp RelationExp ')'
         --AggOpName '(' RelationExp Exp ')'
         -- Nadic Count Etc?

-- TODO: This should be OPTIONAL heading, not a mandatory heading with braces required around it.
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
