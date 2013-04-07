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
        where           { WhereTok         }
        group           { GroupTok         }
        ungroup         { UngroupTok       }
        tclose          { TcloseTok        }
        minus           { MinusTok         }
        i_minus         { IminusTok        }
        not             { NotTok           }
        matching        { MatchingTok      }
        divideby        { DividebyTok      }
        summarize       { SummarizeTok     }
        per             { PerTok           }
        by              { ByTok            }
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
--        '['             { LeftSquareTok    }
--        ']'             { RightSquareTok   }
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

MonadicOrDyadicOtherBuiltInRelationOpInv
    : MonadicOtherBuiltInRelationOpInv                              { MonadicOrDyadicOtherBuiltInRelationOpInv $1 }
    | DyadicOtherBuiltInRelationOpInv                               { MonadicOrDyadicOtherBuiltInRelationOpInv $1 }

MonadicOtherBuiltInRelationOpInv : Rename                           { MonadicOtherBuiltInRelationOpInv $1 }
                                 | Where                            { MonadicOtherBuiltInRelationOpInv $1 }
                                 | Extend                           { MonadicOtherBuiltInRelationOpInv $1 }
                                 | Wrap                             { MonadicOtherBuiltInRelationOpInv $1 }
                                 | Unwrap                           { MonadicOtherBuiltInRelationOpInv $1 }
                                 | Group                            { MonadicOtherBuiltInRelationOpInv $1 }
                                 | Ungroup                          { MonadicOtherBuiltInRelationOpInv $1 }
                                 | Tclose                           { MonadicOtherBuiltInRelationOpInv $1 }

Rename : RelationExp rename '{' RenamingCommalist '}'               { Rename $1 $4  }
Where : RelationExp where BoolExp                                   { Where $1 $3   }
Extend : extend RelationExp ':' '{' AttributeAssignCommalist '}'    { Extend $2 $5  }
Wrap : RelationExp wrap '(' Wrapping ')'                            { Wrap $1 $4    }
Unwrap : RelationExp unwrap '(' Unwrapping ')'                      { Unwrap $1 $4  }
Group : RelationExp group '(' Grouping ')'                          { Group $1 $4   }
Ungroup : RelationExp ungroup '(' Ungrouping ')'                    { Ungroup $1 $4 }
Tclose : tclose '(' RelationExp ')'                                 { Tclose $3     }

-- BoolExp : ?

Grouping : '{' AttributeRefCommalist '}'                            { Grouping $2       }
         | '{' all but AttributeRefCommalist '}'                    { GroupingAllBut $4 }

Ungrouping : AttributeRef                                           { Ungrouping $1 }

DyadicOtherBuiltInRelationOpInv : DyadicUnion                       { DyadicOtherBuiltInRelationOpInvDyadicUnion $1 }
                                | DyadicDisjointUnion               { DyadicOtherBuiltInRelationOpInvDyadicDisjointUnion $1 }
                                | DyadicIntersect                   { DyadicOtherBuiltInRelationOpInvDyadicIntersect $1 }
                                | Minus                             { DyadicOtherBuiltInRelationOpInvMinus $1 }
                                | IncludedMinus                     { DyadicOtherBuiltInRelationOpInvIncludedMinus $1 }
                                | DyadicJoin                        { DyadicOtherBuiltInRelationOpInvDyadicJoin $1 }
                                | DyadicTimes                       { DyadicOtherBuiltInRelationOpInvDyadicTimes $1 }
                                | DyadicXunion                      { DyadicOtherBuiltInRelationOpInvDyadicXunion $1 }
                                | DyadicCompose                     { DyadicOtherBuiltInRelationOpInvDyadicCompose $1 }
                                | Matching                          { DyadicOtherBuiltInRelationOpInvMatching $1 }
                                | NotMatching                       { DyadicOtherBuiltInRelationOpInvNotMatching $1 }
                                | Divide                            { DyadicOtherBuiltInRelationOpInvDivide $1 }
                                | Summarize                         { DyadicOtherBuiltInRelationOpInvSummarize $1 }

DyadicUnion : RelationExp union RelationExp                         { DyadicUnion $1 $3 }
DyadicDisjointUnion : RelationExp d_union RelationExp               { DyadicDisjointUnion $1 $3 }
DyadicIntersect : RelationExp intersect RelationExp                 { DyadicIntersect $1 $3 }
Minus : RelationExp minus RelationExp                               { Minus $1 $3 }
IncludedMinus : RelationExp i_minus RelationExp                     { IncludedMinus $1 $3 }
DyadicJoin : RelationExp join RelationExp                           { DyadicJoin $1 $3 }
DyadicTimes : RelationExp times RelationExp                         { DyadicTimes $1 $3 }
DyadicXunion : RelationExp union RelationExp                        { DyadicXunion $1 $3 }
DyadicCompose : RelationExp compose RelationExp                     { DyadicCompose $1 $3 }
Matching : RelationExp matching RelationExp                         { Matching $1 $3 }
NotMatching : RelationExp not matching RelationExp                  { NotMatching $1 $4 }
Divide : RelationExp divideby RelationExp Per                       { Divide $1 $3 $4 }
Summarize : summarize RelationExp ':' '{' AttributeAssignCommalist '}'           { Summarize $2 $5           }
          | summarize RelationExp PerOrBy ':' '{' AttributeAssignCommalist '}'   { SummarizePerOrBy $2 $3 $6 }

Per : per '(' RelationExp ')'                                       { Per $3 }
    --| per '(' RelationExp ... many

PerOrBy : Per                                                       { PerOrBy $1 }
        | By                                                        { PerOrBy $1 }

-- Note: I inserted By as it's own element, the original grammar had it nested underPerOrBy.
By : by '{' AttributeRefCommalist '}'                               { By $1 }
   | by '{' all but AttributeRefCommalist '}'                       { ByAllBut $1 }

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

RelationSelectorInv : relation '{' TupleExpCommalist '}'                   { RelationSelectorInv $1 $2 }
                    | relation Heading '{' TupleExpCommalist '}'           { RelationSelectorInv $1 $2 }
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
