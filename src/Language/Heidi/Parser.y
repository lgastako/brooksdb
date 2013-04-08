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
        begin           { BeginTok         }
        transaction     { TransactionTok   }
        call            { CallTok          }
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
        prefix          { PrefixTok        }
        suffix          { SuffixTok        }
        key             { KeyTok           }
        integer         { IntegerTok       }
        rational        { RationalTok      }
        character       { CharacterTok     }
        boolean         { BooleanTok       }
        real            { RealTok          }
        base            { BaseTok          }
        relation        { RelationTok      }
        table_dee       { TableDeeTok      }
        table_dum       { TableDumTok      }
        true            { TrueTok          }
        false           { FalseTok         }
        private         { PrivateTok       }
        public          { PublicTok        }
        count           { CountTok         }
        array           { ArrayTok         }
        update          { UpdateTok        }
        insert          { InsertTok        }
        delete          { DeleteTok        }
        i_delete        { IdeleteTok       }
        d_insert        { DinsertTok       }
        same_type_as    { SameTypeAsTok    }
        same_heading_as { SameHeadingAsTok }
        with            { WithTok          }
        '('             { LeftRoundTok     }
        ')'             { RightRoundTok    }
        '{'             { LeftCurlyTok     }
        '}'             { RightCurlyTok    }
        ':'             { ColonTok         }
        ','             { CommaTok         }
        ':='            { AssignerTok      }

--      Parameterized tokens
        varName         { IdentTok $$      }
        int             { IntTok $$        }
        strLiteral      { StrTok $$        }

%%

RealRelationVarDef : var RelationVarName RealOrBase
                         RelationTypeOrInitValue KeyDefList         { RealRelationVarDef $2 $3 $4 $5 }

ApplicationRelationVarDef : var RelationVarName PrivateOrPublic
                                RelationTypeOrInitValue KeyDefList  { ApplicationRelationVarDef $2 $3 $4 $5 }

PrivateOrPublic: private                                            { PrivateOrPublicPrivate }
               | public                                             { PrivateOrPublicPublic }

KeyDefList : KeyDef                                                 { KeyDefList $1 }
           | KeyDefList ',' KeyDef                                  { KeyDefListCons $1 $3 }

KeyDef : key '{' AttributeRefCommalist '}'                          { KeyDef $3 }
       | key '{' all but AttributeRefCommalist '}'                  { KeyDefAllBut $5 }

RealOrBase : real                                                   { RealOrBaseReal }
           | base                                                   { RealOrBaseBase }

RelationVarName : varName                                           { RelationVarName $1 }

RelationTypeOrInitValue : RelationTypeSpec                          { RelationTypeOrInitValueRelationTypeSpec $1        }
                        | init '(' RelationExp ')'                  { RelationTypeOrInitValueInit $3                    }
                        | RelationTypeSpec init '(' RelationExp ')' { RelationTypeOrInitValueRelationTypeSpecInit $1 $4 }

RelationTypeSpec : RelationTypeName                                 { RelationTypeSpecRelationTypeName $1 }
                 | same_type_as '(' RelationExp ')'                 { RelationTypeSpecSameTypeAs $3       }
                 | relation same_heading_as '(' NonscalarExp ')'    { RelationTypeSpecSameHeadingAs $4    }

RelationTypeName : relation Heading                                 { RelationTypeName $2 }

Heading : '{' AttributeCommalist '}'                                { Heading $2 }

AttributeCommalist : Attribute                                      { AttributeCommalist $1 }
                   | AttributeCommalist ',' Attribute               { AttributeCommalistCons $1 $3 }

Attribute : AttributeName TypeSpec                                  { Attribute $1 $2 }

TypeSpec : ScalarTypeSpec                                           { TypeSpecScalar $1 }
         | NonscalarTypeSpec                                        { TypeSpecNonscalar $1 }

ScalarTypeSpec : ScalarTypeName                                     { ScalarTypeSpecScalarTypeName $1 }
               | same_type_as '(' ScalarExp ')'                     { ScalarTypeSpecSameTypeAs $3 }

ScalarTypeName : UserScalarTypeName                                 { ScalarTypeNameUser $1 }
               | BuiltInScalarTypeName                              { ScalarTypeNameBuiltIn $1 }

UserScalarTypeName : varName                                        { UserScalarTypeName $1 }

BuiltInScalarTypeName : integer                                     { BuiltInScalarTypeNameInteger }
                      | rational                                    { BuiltInScalarTypeNameRational }
                      | character                                   { BuiltInScalarTypeNameCharacter }
                      | boolean                                     { BuiltInScalarTypeNameBoolean }

NonscalarTypeSpec : TupleTypeSpec                                   { NonscalarTypeSpecTupleTypeSpec $1 }
                  | RelationTypeSpec                                { NonscalarTypeSpecRelationTypeSpec $1 }

TupleTypeSpec : TupleTypeName                                       { TupleTypeSpecTupleTypeName $1 }
              | same_type_as '(' TupleExp ')'                       { TupleTypeSpecSameTypeAs $3 }
              | tuple same_heading_as '(' NonscalarExp ')'          { TupleTypeSpecSameHeadingAs $4 }

TupleTypeName : tuple Heading                                       { TupleTypeName $2 }

NonscalarExp : TupleExp                                             { NonscalarExpTupleExp $1 }
             | RelationExp                                          { NonscalarExpRelationExp $1 }

TupleExp : TupleWithExp                                             { TupleExpWith $1 }
         | TupleNonwithExp                                          { TupleExpNonwith $1 }

TupleWithExp : with '(' NameIntroCommalist ')' ':' TupleExp         { TupleWithExp $3 $6 }

TupleNonwithExp : TupleVarRef                                       { TupleNonwithExpTupleVarRef $1 }
                | TupleOpInv                                        { TupleNonwithExpTupleOpInv $1 }
                | ArrayVarRef '(' Subscript ')'                     { TupleNonwithExpArrayVarRefSubscript $1 $3 }
                | '(' TupleExp ')'                                  { TupleNonwithExpNestedTupleExp $2 }

NameIntroCommalist : NameIntro                                      { NameIntroCommalist $1 }
                   | NameIntroCommalist ',' NameIntro               { NameIntroCommalistCons $1 $3 }

NameIntro : IntroducedName ':=' Exp                                 { NameIntro $1 $3 }

ArrayVarRef : ArrayVarName                                          { ArrayVarRef $1 }

ArrayVarName : varName                                              { ArrayVarName $1 }

TupleVarRef : TupleVarName                                          { TupleVarRef $1 }

TupleVarName : varName                                              { TupleVarName $1 }

TupleOpInv : UserOpInv                                              { TupleOpInvUser $1 }
           | BuiltInTupleOpInv                                      { TupleOpInvBuiltIn $1 }

BuiltInTupleOpInv : TupleSelectorInv                                { BuiltInTupleOpInvTupleSelectorInv $1 }
                  | THE_OpInv                                       { BuiltInTupleOpInvTHE_OpInv $1 }
                  | AttributeExtractorInv                           { BuiltInTupleOpInvAttributeExtractorInv $1 }
                  | TupleExtractorInv                               { BuiltInTupleOpInvTupleExtractorInv $1 }
                  | TupleProject                                    { BuiltInTupleOpInvTupleProject $1 }
                  | NadicOtherBuiltInTupleOpInv                     { BuiltInTupleOpInvNadicOtherBuiltInTupleOpInv $1 }
                  | MonadicOrDyadicOtherBuiltInTupleOpInv           { BuiltInTupleOpInvMonadicOrDyadicOtherBuiltInTupleOpInv $1 }

TupleSelectorInv : tuple '{' TupleComponentCommalist '}'            { TupleSelectorInv $3 }

TupleComponentCommalist : TupleComponent                             { TupleComponentCommalist $1 }
                        | TupleComponentCommalist ',' TupleComponent { TupleComponentCommalistCons $1 $3 }

TupleComponent : AttributeName Exp                                  { TupleComponent $1 $2 }

AttributeExtractorInv : AttributeRef from TupleExp                  { AttributeExtractorInv $1 $3 }

AttributeRef : AttributeName                                        { AttributeRef $1 }

AttributeRefCommalist : AttributeRef                                { AttributeRefCommalist $1 }
                      | AttributeRefCommalist ',' AttributeRef      { AttributeRefCommalistCons $1 $3 }

AttributeName : varName                                             { AttributeName $1 }

TupleExtractorInv : tuple from RelationExp                          { TupleExtractorInv $3 }

TupleProject : TupleExp '{' AttributeRefCommalist '}'               { TupleProject $1 $3 }
             | TupleExp '{' all but AttributeRefCommalist '}'       { TupleProjectAllBut $1 $5 }

NadicOtherBuiltInTupleOpInv : NadicTupleUnion                       { NadicOtherBuiltInTupleOpInv $1 }

NadicTupleUnion : union '{' TupleExpCommalist '}'                   { NadicTupleUnion $3 }

TupleExpCommalist : TupleExp                                        { TupleExpCommalist $1 }
                  | TupleExpCommalist ',' TupleExp                  { TupleExpCommalistCons $1 $3 }

MonadicOrDyadicOtherBuiltInTupleOpInv : MonadicOtherBuiltInTupleOpInv { MonadicOrDyadicOtherBuiltInTupleOpInvMonadic $1 }
                                      | DyadicOtherBuiltInTupleOpInv  { MonadicOrDyadicOtherBuiltInTupleOpInvDyadic $1 }

MonadicOtherBuiltInTupleOpInv : TupleRename                         { MonadicOtherBuiltInTupleOpInvRename $1 }
                              | TupleExtend                         { MonadicOtherBuiltInTupleOpInvExtend $1 }
                              | TupleWrap                           { MonadicOtherBuiltInTupleOpInvWrap $1 }
                              | TupleUnwrap                         { MonadicOtherBuiltInTupleOpInvUnwrap $1 }

DyadicOtherBuiltInTupleOpInv : DyadicTupleUnion                     { DyadicOtherBuiltInTupleOpInvUnion $1 }
                             | DyadicTupleCompose                   { DyadicOtherBuiltInTupleOpInvCompose $1 }

DyadicTupleUnion : TupleExp union TupleExp                          { DyadicTupleUnion $1 $3 }

DyadicTupleCompose : TupleExp compose TupleExp                      { DyadicTupleCompose $1 $3 }

TupleRename : TupleExp rename '{' RenamingCommalist '}'             { TupleRename $1 $4 }

TupleExtend : extend TupleExp ':' '{' AttributeAssignCommalist '}'  { TupleExtend $2 $5 }

RenamingCommalist : Renaming                                        { RenamingCommalist $1 }
                  | RenamingCommalist ',' Renaming                  { RenamingCommalistCons $1 $3 }

Renaming : AttributeRef as IntroducedName                           { Renaming $1 $3 }
         | prefix CharacterStringLiteral
               as CharacterStringLiteral                            { RenamingPrefix $2 $4 }
         | suffix CharacterStringLiteral
               as CharacterStringLiteral                            { RenamingSuffix $2 $4 }

CharacterStringLiteral : strLiteral                                 { CharacterStringLiteral $1 }

AttributeAssignCommalist : AttributeAssign                              { AttributeAssignCommalist $1 }
                         | AttributeAssignCommalist ',' AttributeAssign { AttributeAssignCommalistCons $1 $3 }

-- Inferred from TTMHC's <extend add>
-- TODO: Looks like the online stuff and book are not in sync.  Sync up I guess.  Hopefully pretty close.
AttributeAssign : Exp as IntroducedName                             { AttributeAssign $1 $3 }

TupleWrap : TupleExp wrap '(' Wrapping ')'                          { TupleWrap $1 $4 }

TupleUnwrap : TupleExp unwrap '(' Unwrapping ')'                    { TupleUnwrap $1 $4 }

Wrapping : '{' AttributeRefCommalist '}' as IntroducedName          { Wrapping $2 $5 }
         | '{' all but AttributeRefCommalist '}' as IntroducedName  { WrappingAllBut $4 $7 }

IntroducedName : varName                                            { IntroducedName $1 }

Unwrapping : AttributeRef                                           { Unwrapping $1 }

Subscript : IntegerExp                                              { Subscript $1 }

IntegerExp : int                                                    { IntegerExp $1 }

RelationExp : RelationWithExp                                       { RelationExpWith $1 }
            | RelationNonwithExp                                    { RelationExpNonwith $1 }

RelationWithExp : with '(' NameIntroCommalist ')' ':' RelationExp   { RelationWithExp $3 $6 }

RelationNonwithExp : RelationVarRef                                 { RelationNonwithExpRelationVarRef $1 }
                   | RelationOpInv                                  { RelationNonwithExpRelationOpInv $1  }
                   | '(' RelationExp ')'                            { RelationNonwithExpRelationExp $2    }

RelationVarRef : RelationVarName                                    { RelationVarRef $1 }

RelationOpInv : UserOpInv                                           { RelationOpInvUserOpInv $1 }
              | BuiltInRelationOpInv                                { RelationOpInvBuiltInRelationOpInv $1 }

UserOpInv : UserOpName '(' ArgumentExpCommalist ')'                 { UserOpInv $1 $3 }

UserOpName : varName                                                { UserOpName $1 }

BuiltInRelationOpInv : RelationSelectorInv                          { BuiltInRelationOpInvRelationSelectorInv $1                      }
                     | THE_OpInv                                    { BuiltInRelationOpInvTHE_OpInv $1                                }
                     | AttributeExtractorInv                        { BuiltInRelationOpInvAttributeExtractorInv $1                    }
                     | Project                                      { BuiltInRelationOpInvProject $1                                  }
                     | NadicOtherBuiltInRelationOpInv               { BuiltInRelationOpInvNadicOtherBuiltInRelationOpInv $1           }
                     | MonadicOrDyadicOtherBuiltInRelationOpInv     { BuiltInRelationOpInvMonadicOrDyadicOtherBuiltInRelationOpInv $1 }

THE_OpInv : THE_OpName '(' ScalarExp ')'                            { THE_OpInv $1 $3 }

THE_OpName : varName                                                { THE_OpName $1 }

Project : RelationExp '{' AttributeRefCommalist '}'                 { Project $1 $3       }
        | RelationExp '{' all but AttributeRefCommalist '}'         { ProjectAllBut $1 $5 }

NadicOtherBuiltInRelationOpInv : NadicUnion                         { NadicOtherBuiltInRelationOpInvUnion $1         }
                               | NadicDisjointUnion                 { NadicOtherBuiltInRelationOpInvDisjointUnion $1 }
                               | NadicIntersect                     { NadicOtherBuiltInRelationOpInvIntersect $1     }
                               | NadicJoin                          { NadicOtherBuiltInRelationOpInvJoin $1          }
                               | NadicTimes                         { NadicOtherBuiltInRelationOpInvTimes $1         }
                               | NadicXunion                        { NadicOtherBuiltInRelationOpInvXunion $1        }
                               | NadicCompose                       { NadicOtherBuiltInRelationOpInvCompose $1       }

NadicUnion : union '{' RelationExpCommalist '}'                     { NadicUnion $3          }
           | union Heading '{' RelationExpCommalist '}'             { NadicUnionHeaded $2 $4 }

NadicDisjointUnion : d_union '{' RelationExpCommalist '}'           { NadicDisjointUnion $3          }
                   | d_union Heading '{' RelationExpCommalist '}'   { NadicDisjointUnionHeaded $2 $4 }

NadicIntersect : intersect '{' RelationExpCommalist '}'             { NadicIntersect $3          }
               | intersect Heading '{' RelationExpCommalist '}'     { NadicIntersectHeaded $2 $4 }

NadicJoin : join '{' RelationExpCommalist '}'                       { NadicJoin $3 }

NadicTimes : join '{' RelationExpCommalist '}'                      { NadicTimes $3 }

NadicXunion : xunion '{' RelationExpCommalist '}'                   { NadicXunion $3          }
            | xunion Heading '{' RelationExpCommalist '}'           { NadicXunionHeaded $2 $4 }

NadicCompose : join '{' RelationExpCommalist '}'                    { NadicCompose $3 }

RelationExpCommalist : RelationExp                                  { RelationExpCommalist $1 }
                     | RelationExpCommalist ',' RelationExp         { RelationExpCommalistCons $1 $3 }

MonadicOrDyadicOtherBuiltInRelationOpInv
    : MonadicOtherBuiltInRelationOpInv                              { MonadicOrDyadicOtherBuiltInRelationOpInvMonadic $1 }
    | DyadicOtherBuiltInRelationOpInv                               { MonadicOrDyadicOtherBuiltInRelationOpInvDyadic $1 }

MonadicOtherBuiltInRelationOpInv : Rename                           { MonadicOtherBuiltInRelationOpInvRename $1 }
                                 | Where                            { MonadicOtherBuiltInRelationOpInvWhere $1 }
                                 | Extend                           { MonadicOtherBuiltInRelationOpInvExtend $1 }
                                 | Wrap                             { MonadicOtherBuiltInRelationOpInvWrap $1 }
                                 | Unwrap                           { MonadicOtherBuiltInRelationOpInvUnwrap $1 }
                                 | Group                            { MonadicOtherBuiltInRelationOpInvGroup $1 }
                                 | Ungroup                          { MonadicOtherBuiltInRelationOpInvUngroup $1 }
                                 | Tclose                           { MonadicOtherBuiltInRelationOpInvTclose $1 }

Rename : RelationExp rename '{' RenamingCommalist '}'               { Rename $1 $4  }
Where : RelationExp where BoolExp                                   { Where $1 $3   }
Extend : extend RelationExp ':' '{' AttributeAssignCommalist '}'    { Extend $2 $5  }
Wrap : RelationExp wrap '(' Wrapping ')'                            { Wrap $1 $4    }
Unwrap : RelationExp unwrap '(' Unwrapping ')'                      { Unwrap $1 $4  }
Group : RelationExp group '(' Grouping ')'                          { Group $1 $4   }
Ungroup : RelationExp ungroup '(' Ungrouping ')'                    { Ungroup $1 $4 }
Tclose : tclose '(' RelationExp ')'                                 { Tclose $3     }

-- JUST FOR NOW.
BoolExp : true                                                      { BoolExpTrue  }
        | false                                                     { BoolExpFalse }

Grouping : '{' AttributeRefCommalist '}'                            { Grouping $2       }
         | '{' all but AttributeRefCommalist '}'                    { GroupingAllBut $4 }

Ungrouping : AttributeRef                                           { Ungrouping $1 }

DyadicOtherBuiltInRelationOpInv : DyadicUnion                       { DyadicOtherBuiltInRelationOpInvDyadicUnion $1         }
                                | DyadicDisjointUnion               { DyadicOtherBuiltInRelationOpInvDyadicDisjointUnion $1 }
                                | DyadicIntersect                   { DyadicOtherBuiltInRelationOpInvDyadicIntersect $1     }
                                | Minus                             { DyadicOtherBuiltInRelationOpInvMinus $1               }
                                | IncludedMinus                     { DyadicOtherBuiltInRelationOpInvIncludedMinus $1       }
                                | DyadicJoin                        { DyadicOtherBuiltInRelationOpInvDyadicJoin $1          }
                                | DyadicTimes                       { DyadicOtherBuiltInRelationOpInvDyadicTimes $1         }
                                | DyadicXunion                      { DyadicOtherBuiltInRelationOpInvDyadicXunion $1        }
                                | DyadicCompose                     { DyadicOtherBuiltInRelationOpInvDyadicCompose $1       }
                                | Matching                          { DyadicOtherBuiltInRelationOpInvMatching $1            }
                                | NotMatching                       { DyadicOtherBuiltInRelationOpInvNotMatching $1         }
                                | Divide                            { DyadicOtherBuiltInRelationOpInvDivide $1              }
                                | Summarize                         { DyadicOtherBuiltInRelationOpInvSummarize $1           }

DyadicUnion : RelationExp union RelationExp                         { DyadicUnion $1 $3         }
DyadicDisjointUnion : RelationExp d_union RelationExp               { DyadicDisjointUnion $1 $3 }
DyadicIntersect : RelationExp intersect RelationExp                 { DyadicIntersect $1 $3     }
Minus : RelationExp minus RelationExp                               { Minus $1 $3               }
IncludedMinus : RelationExp i_minus RelationExp                     { IncludedMinus $1 $3       }
DyadicJoin : RelationExp join RelationExp                           { DyadicJoin $1 $3          }
DyadicTimes : RelationExp times RelationExp                         { DyadicTimes $1 $3         }
DyadicXunion : RelationExp union RelationExp                        { DyadicXunion $1 $3        }
DyadicCompose : RelationExp compose RelationExp                     { DyadicCompose $1 $3       }
Matching : RelationExp matching RelationExp                         { Matching $1 $3            }
NotMatching : RelationExp not matching RelationExp                  { NotMatching $1 $4         }
Divide : RelationExp divideby RelationExp Per                       { Divide $1 $3 $4           }

Summarize : summarize RelationExp ':' '{' AttributeAssignCommalist '}'           { Summarize $2 $5           }
          | summarize RelationExp PerOrBy ':' '{' AttributeAssignCommalist '}'   { SummarizePerOrBy $2 $3 $6 }

Per : per '(' RelationExp ')'                                       { Per $3 }
    --| per '(' RelationExp ... many

PerOrBy : Per                                                       { PerOrByPer $1 }
        | By                                                        { PerOrByBy $1  }

-- Note: I inserted By as it's own element, the original grammar had it nested underPerOrBy.
By : by '{' AttributeRefCommalist '}'                               { By $3 }
   | by '{' all but AttributeRefCommalist '}'                       { ByAllBut $5 }

ScalarExp : ScalarWithExp                                           { ScalarExpWith $1    }
          | ScalarNonwithExp                                        { ScalarExpNonwith $1 }

ScalarWithExp : with '(' NameIntroCommalist ')' ':' ScalarExp       { ScalarWithExp $3 $6 }

ScalarNonwithExp : ScalarVarRef                                     { ScalarNonwithExpScalarVarRef $1 }
                 | ScalarOpInv                                      { ScalarNonwithExpScalarOpInv $1  }
                 | '(' ScalarExp ')'                                { ScalarNonwithExpScalarExp $2    }

ScalarVarRef : ScalarVarName                                        { ScalarVarRef $1 }

ScalarVarName : varName                                             { ScalarVarName $1 }

ScalarOpInv : UserOpInv                                             { ScalarOpInvUserOpInv $1          }
            | BuiltInScalarOpInv                                    { ScalarOpInvBuiltInScalarOpInv $1 }

BuiltInScalarOpInv : ScalarSelectorInv                              { BuiltInScalarOpInvScalarSelectorInv $1     }
                   | THE_OpInv                                      { BuiltInScalarOpInvTHE_OpInv $1             }
                   | AttributeExtractorInv                          { BuiltInScalarOpInvAttributeExtractorInv $1 }
                   --| AggOpInv                                       { BuiltInScalarOpInv $1 }
                   -- "plus the usual possibilities...eh?"

--ScalarSelectorInv : BuiltInScalarLiteral                            { ScalarSelectorInvBuiltInScalarLiteral $1 }
--                  | PossrepName '(' ArgumentExpCommalist ')'        { ScalarSelectorInvPossrepName $1 $3 }

ScalarSelectorInv : PossrepName '(' ArgumentExpCommalist ')'         { ScalarSelectorInvPossrepName $1 $3 }

-- BuiltInScalarLiteral : ?

ArgumentExpCommalist : ArgumentExp                                  { ArgumentExpCommalist $1 }
                     | ArgumentExpCommalist ',' ArgumentExp         { ArgumentExpCommalistCons $1 $3 }

ArgumentExp : Exp                                                   { ArgumentExp $1 }

Exp : ScalarExp                                                     { ExpScalar $1    }
    | NonscalarExp                                                  { ExpNonscalar $1 }

PossrepName : varName                                               { PossrepName $1 }

--AggOpInv : AggOpName '(' RelationExp ')'
           --AggOpName '(' IntegerExp RelationExp ')'
         --AggOpName '(' RelationExp Exp ')'
         -- Nadic Count Etc?

RelationSelectorInv : relation '{' TupleExpCommalist '}'            { RelationSelectorInv $3 }
                    | relation Heading '{' TupleExpCommalist '}'    { RelationSelectorInvHeaded $2 $4 }
                    | table_dee                                     { RelationSelectorInvTableDee }
                    | table_dum                                     { RelationSelectorInvTableDum }

ArrayCardinality : count '(' ArrayVarRef ')'                        { ArrayCardinality $3 }

ArrayTarget : ArrayVarRef                                           { ArrayTarget $1 }

ArrayVarDef : var ArrayVarName array TupleTypeSpec                  { ArrayVarDef $2 $4 }

Assign : ScalarAssign                                               { AssignScalar $1 }
       | NonscalarAssign                                            { AssignNonscalar $1 }

ScalarAssign : ScalarTarget ':=' ScalarExp                          { ScalarAssignTarget $1 $3 }
             | ScalarUpdate                                         { ScalarAssignUpdate $1 }

ScalarUpdate : update ScalarTarget
                      '{' PossrepComponentAssignCommalist '}'       { ScalarUpdate $2 $4 }

ScalarTarget : ScalarVarRef                                         { ScalarTargetVarRef $1 }
             | ScalarTHE_PvRef                                      { ScalarTargetTHE_PvRef $1 }

ScalarTHE_PvRef : THE_PvName '(' ScalarTarget ')'                   { ScalarTHE_PvRef $1 $3 }

PossrepComponentAssignCommalist
    : PossrepComponentAssign                                        { PossrepComponentAssignCommalist $1 }
    | PossrepComponentAssignCommalist ',' PossrepComponentAssign    { PossrepComponentAssignCommalistCons $1 $3 }

-- Not listed, not putting junk to fill the hole for now.
PossrepComponentAssign : varName                                    { PossrepComponentAssignFake $1 }

NonscalarAssign : TupleAssign                                       { NonscalarAssignTuple $1 }
                | RelationAssign                                    { NonscalarAssignRelation $1 }

TupleAssign : TupleTarget ':=' TupleExp                             { TupleAssignTarget $1 $3 }
            | TupleUpdate                                           { TupleAssignUpdate $1 }

TupleUpdate : update TupleTarget '{' AttributeAssignCommalist '}'   { TupleUpdate $2 $4 }

TupleTarget : TupleVarRef                                           { TupleTargetVarRef $1 }
            | TupleTHE_PvRef                                        { TupleTargetTHE_PvRef $1 }

TupleTHE_PvRef : THE_PvName '(' ScalarTarget ')'                    { TupleTHE_PvRef $1 $3 }

THE_PvName : varName                                                { THE_PvName $1 }

RelationAssign : RelationTarget ':=' RelationExp                    { RelationAssignTarget $1 $3 }
               | RelationInsert                                     { RelationAssignInsert $1    }
               | RelationDinsert                                    { RelationAssignDinsert $1   }
               | RelationDelete                                     { RelationAssignDelete $1    }
               | RelationIdelete                                    { RelationAssignIdelete $1   }
               | RelationUpdate                                     { RelationAssignUpdate $1    }

RelationTarget : RelationVarRef                                     { RelationTargetVarRef $1    }
               | RelationTHE_PvRef                                  { RelationTargetTHE_PvRef $1 }

RelationTHE_PvRef : THE_PvName '(' ScalarTarget ')'                 { RelationTHE_PvRef $1 $3 }

RelationInsert : insert RelationTarget RelationExp                  { RelationInsert $2 $3 }

RelationDinsert : d_insert RelationTarget RelationExp               { RelationDinsert $2 $3 }

RelationDelete : delete RelationTarget RelationExp                  { RelationDelete $2 $3 }
               | delete RelationTarget where BoolExp                { RelationDeleteWhere $2 $4 }

RelationIdelete : i_delete RelationTarget RelationExp               { RelationIdelete $2 $3 }

RelationUpdate : update RelationTarget
                        '{' AttributeAssignCommalist '}'            { RelationUpdate $2 $4 }
               | update RelationTarget where BoolExp
                        '{' AttributeAssignCommalist '}'            { RelationUpdateWhere $2 $4 $6 }

Assignment : AssignCommalist Attribute                              { Assignment $1 $2 }

AssignCommalist : Assign                                            { AssignCommalist $1 }
                | AssignCommalist ',' Assign                        { AssignCommalistCons $1 $3 }

AttributeTarget : AttributeRef                                      { AttributeTargetRef $1 }
                | AttributeTHE_PvRef                                { AttributeTargetTHE_PvRef $1 }

AttributeTHE_PvRef : THE_PvName '(' AttributeTarget ')'             { AttributeTHE_PvRef $1 $3 }

BeginTransaction : begin transaction                                { BeginTransaction }

Call : call UserOpInv                                               { Call $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data PossrepName = PossrepName Identifier
    deriving (Show)

data Exp = ExpScalar ScalarExp
         | ExpNonscalar NonscalarExp
    deriving (Show)

data ScalarExp = ScalarExpWith ScalarWithExp
               | ScalarExpNonwith ScalarNonwithExp
    deriving (Show)

data ScalarWithExp = ScalarWithExp NameIntroCommalist ScalarExp
    deriving (Show)

data NonscalarExp = NonscalarExpTupleExp TupleExp
                  | NonscalarExpRelationExp RelationExp
    deriving (Show)

data By = By AttributeRefCommalist
        | ByAllBut AttributeRefCommalist
    deriving (Show)

data AttributeRefCommalist = AttributeRefCommalist AttributeRef
                           | AttributeRefCommalistCons AttributeRefCommalist AttributeRef
    deriving (Show)

data AttributeRef = AttributeRef AttributeName
    deriving (Show)

data AttributeName = AttributeName Identifier
    deriving (Show)

data NameIntroCommalist = NameIntroCommalist NameIntro
                        | NameIntroCommalistCons NameIntroCommalist NameIntro
    deriving (Show)

data NameIntro = NameIntro IntroducedName Exp
    deriving (Show)

data IntroducedName = IntroducedName Identifier
    deriving (Show)

data ScalarNonwithExp = ScalarNonwithExpScalarVarRef ScalarVarRef
                      | ScalarNonwithExpScalarOpInv ScalarOpInv
                      | ScalarNonwithExpScalarExp ScalarExp
    deriving (Show)

data ScalarOpInv = ScalarOpInvUserOpInv UserOpInv
                 | ScalarOpInvBuiltInScalarOpInv BuiltInScalarOpInv
    deriving (Show)

data BuiltInScalarOpInv = BuiltInScalarOpInvScalarSelectorInv ScalarSelectorInv
                        | BuiltInScalarOpInvTHE_OpInv THE_OpInv
                        | BuiltInScalarOpInvAttributeExtractorInv AttributeExtractorInv
    deriving (Show)

data UserOpInv = UserOpInv UserOpName ArgumentExpCommalist
    deriving (Show)

data ArgumentExpCommalist = ArgumentExpCommalist ArgumentExp
                          | ArgumentExpCommalistCons ArgumentExpCommalist ArgumentExp
    deriving (Show)

data ArgumentExp = ArgumentExp Exp
    deriving (Show)

data UserOpName = UserOpName Identifier
    deriving (Show)

data ScalarVarRef = ScalarVarRef ScalarVarName
    deriving (Show)

data ScalarVarName = ScalarVarName Identifier
    deriving (Show)

data RelationSelectorInv = RelationSelectorInv TupleExpCommalist
                         | RelationSelectorInvHeaded Heading TupleExpCommalist
                         | RelationSelectorInvTableDee
                         | RelationSelectorInvTableDum
    deriving (Show)

data Heading = Heading AttributeCommalist
    deriving (Show)

data AttributeCommalist = AttributeCommalist Attribute
                        | AttributeCommalistCons AttributeCommalist Attribute
    deriving (Show)

data Attribute = Attribute AttributeName TypeSpec
    deriving (Show)

data TypeSpec = TypeSpecScalar ScalarTypeSpec
              | TypeSpecNonscalar NonscalarTypeSpec
    deriving (Show)

data ScalarTypeSpec = ScalarTypeSpecScalarTypeName ScalarTypeName
                    | ScalarTypeSpecSameTypeAs ScalarExp
    deriving (Show)

data ScalarTypeName = ScalarTypeNameUser UserScalarTypeName
                    | ScalarTypeNameBuiltIn BuiltInScalarTypeName
    deriving (Show)

data TupleExpCommalist = TupleExpCommalist TupleExp
                       | TupleExpCommalistCons TupleExpCommalist TupleExp
    deriving (Show)

data TupleExp = TupleExpWith TupleWithExp
              | TupleExpNonwith TupleNonwithExp
    deriving (Show)

data TupleWithExp = TupleWithExp NameIntroCommalist TupleExp
    deriving (Show)

data TupleNonwithExp = TupleNonwithExpTupleVarRef TupleVarRef
                     | TupleNonwithExpTupleOpInv TupleOpInv
                     | TupleNonwithExpArrayVarRefSubscript ArrayVarRef Subscript
                     | TupleNonwithExpNestedTupleExp TupleExp
    deriving (Show)

data Subscript = Subscript IntegerExp
    deriving (Show)

data ArrayVarRef = ArrayVarRef ArrayVarName
    deriving (Show)

data ArrayVarName = ArrayVarName Identifier
    deriving (Show)

data IntegerExp = IntegerExp Int
    deriving (Show)

data TupleOpInv = TupleOpInvUser UserOpInv
                | TupleOpInvBuiltIn BuiltInTupleOpInv
    deriving (Show)

data BuiltInTupleOpInv = BuiltInTupleOpInvTupleSelectorInv TupleSelectorInv
                       | BuiltInTupleOpInvTHE_OpInv THE_OpInv
                       | BuiltInTupleOpInvAttributeExtractorInv AttributeExtractorInv
                       | BuiltInTupleOpInvTupleExtractorInv TupleExtractorInv
                       | BuiltInTupleOpInvTupleProject TupleProject
                       | BuiltInTupleOpInvNadicOtherBuiltInTupleOpInv NadicOtherBuiltInTupleOpInv
                       | BuiltInTupleOpInvMonadicOrDyadicOtherBuiltInTupleOpInv MonadicOrDyadicOtherBuiltInTupleOpInv
    deriving (Show)

data DyadicOtherBuiltInTupleOpInv = DyadicOtherBuiltInTupleOpInvCompose DyadicTupleCompose
                                  | DyadicOtherBuiltInTupleOpInvUnion DyadicTupleUnion
    deriving (Show)

data TupleVarRef = TupleVarRef TupleVarName
    deriving (Show)

data TupleVarName = TupleVarName Identifier
    deriving (Show)

data Per = Per RelationExp
    deriving (Show)

data PerOrBy = PerOrByPer Per
             | PerOrByBy By
    deriving (Show)

data Summarize = Summarize RelationExp AttributeAssignCommalist
               | SummarizePerOrBy RelationExp PerOrBy AttributeAssignCommalist
    deriving (Show)

data AttributeAssignCommalist = AttributeAssignCommalist AttributeAssign
                              | AttributeAssignCommalistCons AttributeAssignCommalist AttributeAssign
    deriving (Show)

data AttributeAssign = AttributeAssign Exp IntroducedName
    deriving (Show)


data ScalarSelectorInv = ScalarSelectorInvPossrepName PossrepName ArgumentExpCommalist
    deriving (Show)

data Divide = Divide RelationExp RelationExp Per
    deriving (Show)

data Matching = Matching RelationExp RelationExp
    deriving (Show)

data NotMatching = NotMatching RelationExp RelationExp
    deriving (Show)

data RelationExp = RelationExpWith RelationWithExp
                 | RelationExpNonwith RelationNonwithExp
    deriving (Show)

data Minus = Minus RelationExp RelationExp
    deriving (Show)

data DyadicJoin = DyadicJoin RelationExp RelationExp
    deriving (Show)

data DyadicTimes = DyadicTimes RelationExp RelationExp
    deriving (Show)

data DyadicIntersect = DyadicIntersect RelationExp RelationExp
    deriving (Show)

data DyadicUnion = DyadicUnion RelationExp RelationExp
    deriving (Show)

data DyadicDisjointUnion = DyadicDisjointUnion RelationExp RelationExp
    deriving (Show)

data DyadicXunion = DyadicXunion RelationExp RelationExp
    deriving (Show)

data DyadicCompose = DyadicCompose RelationExp RelationExp
    deriving (Show)

data RelationWithExp = RelationWithExp NameIntroCommalist RelationExp
    deriving (Show)

data RelationNonwithExp = RelationNonwithExpRelationVarRef RelationVarRef
                        | RelationNonwithExpRelationOpInv RelationOpInv
                        | RelationNonwithExpRelationExp RelationExp
    deriving (Show)

data RelationOpInv = RelationOpInvUserOpInv UserOpInv
                   | RelationOpInvBuiltInRelationOpInv BuiltInRelationOpInv
    deriving (Show)

data BuiltInRelationOpInv = BuiltInRelationOpInvRelationSelectorInv RelationSelectorInv
                          | BuiltInRelationOpInvTHE_OpInv THE_OpInv
                          | BuiltInRelationOpInvAttributeExtractorInv AttributeExtractorInv
                          | BuiltInRelationOpInvProject Project
                          | BuiltInRelationOpInvNadicOtherBuiltInRelationOpInv NadicOtherBuiltInRelationOpInv
                          | BuiltInRelationOpInvMonadicOrDyadicOtherBuiltInRelationOpInv MonadicOrDyadicOtherBuiltInRelationOpInv
    deriving (Show)

data RelationVarRef = RelationVarRef RelationVarName
    deriving (Show)

data RelationVarName = RelationVarName Identifier
    deriving (Show)

data RealRelationVarDef = RealRelationVarDef RelationVarName RealOrBase RelationTypeOrInitValue KeyDefList
    deriving (Show)

data KeyDefList = KeyDefList KeyDef
                | KeyDefListCons KeyDefList KeyDef
    deriving (Show)

data KeyDef = KeyDef AttributeRefCommalist
            | KeyDefAllBut AttributeRefCommalist
    deriving (Show)

data RelationTypeOrInitValue = RelationTypeOrInitValueRelationTypeSpec RelationTypeSpec
                             | RelationTypeOrInitValueInit RelationExp
                             | RelationTypeOrInitValueRelationTypeSpecInit RelationTypeSpec RelationExp
    deriving (Show)

data RelationTypeSpec = RelationTypeSpecRelationTypeName RelationTypeName
                      | RelationTypeSpecSameTypeAs RelationExp
                      | RelationTypeSpecSameHeadingAs NonscalarExp
    deriving (Show)

data RelationTypeName = RelationTypeName Heading
    deriving (Show)

data RealOrBase = RealOrBaseReal
                | RealOrBaseBase
    deriving (Show)

data IncludedMinus = IncludedMinus RelationExp RelationExp
    deriving (Show)

data DyadicOtherBuiltInRelationOpInv = DyadicOtherBuiltInRelationOpInvDyadicUnion DyadicUnion
                                     | DyadicOtherBuiltInRelationOpInvDyadicDisjointUnion DyadicDisjointUnion
                                     | DyadicOtherBuiltInRelationOpInvDyadicIntersect DyadicIntersect
                                     | DyadicOtherBuiltInRelationOpInvMinus Minus
                                     | DyadicOtherBuiltInRelationOpInvIncludedMinus IncludedMinus
                                     | DyadicOtherBuiltInRelationOpInvDyadicJoin DyadicJoin
                                     | DyadicOtherBuiltInRelationOpInvDyadicTimes DyadicTimes
                                     | DyadicOtherBuiltInRelationOpInvDyadicXunion DyadicXunion
                                     | DyadicOtherBuiltInRelationOpInvDyadicCompose DyadicCompose
                                     | DyadicOtherBuiltInRelationOpInvMatching Matching
                                     | DyadicOtherBuiltInRelationOpInvNotMatching NotMatching
                                     | DyadicOtherBuiltInRelationOpInvDivide Divide
                                     | DyadicOtherBuiltInRelationOpInvSummarize Summarize
    deriving (Show)

data Group = Group RelationExp Grouping
    deriving (Show)

data Ungroup = Ungroup RelationExp Ungrouping
    deriving (Show)

data Grouping = Grouping AttributeRefCommalist
              | GroupingAllBut AttributeRefCommalist
    deriving (Show)

data Ungrouping = Ungrouping AttributeRef
    deriving (Show)

data Wrap = Wrap RelationExp Wrapping
    deriving (Show)

data Wrapping = Wrapping AttributeRefCommalist IntroducedName
              | WrappingAllBut AttributeRefCommalist IntroducedName
    deriving (Show)

data Unwrap = Unwrap RelationExp Unwrapping
    deriving (Show)

data Unwrapping = Unwrapping AttributeRef
    deriving (Show)

data Tclose = Tclose RelationExp
    deriving (Show)

data Extend = Extend RelationExp AttributeAssignCommalist
    deriving (Show)

data Rename = Rename RelationExp RenamingCommalist
    deriving (Show)

data RenamingCommalist = RenamingCommalist Renaming
                       | RenamingCommalistCons RenamingCommalist Renaming
    deriving (Show)

data Renaming = Renaming AttributeRef IntroducedName
              | RenamingPrefix CharacterStringLiteral CharacterStringLiteral
              | RenamingSuffix CharacterStringLiteral CharacterStringLiteral
    deriving (Show)

data CharacterStringLiteral = CharacterStringLiteral String
    deriving (Show)

data Where = Where RelationExp BoolExp
    deriving (Show)

data BoolExp = BoolExpTrue
             | BoolExpFalse
    deriving (Show)

data MonadicOtherBuiltInRelationOpInv = MonadicOtherBuiltInRelationOpInvRename Rename
                                      | MonadicOtherBuiltInRelationOpInvWhere Where
                                      | MonadicOtherBuiltInRelationOpInvExtend Extend
                                      | MonadicOtherBuiltInRelationOpInvWrap Wrap
                                      | MonadicOtherBuiltInRelationOpInvUnwrap Unwrap
                                      | MonadicOtherBuiltInRelationOpInvGroup Group
                                      | MonadicOtherBuiltInRelationOpInvUngroup Ungroup
                                      | MonadicOtherBuiltInRelationOpInvTclose Tclose
    deriving (Show)

data MonadicOrDyadicOtherBuiltInRelationOpInv = MonadicOrDyadicOtherBuiltInRelationOpInvMonadic MonadicOtherBuiltInRelationOpInv
                                              | MonadicOrDyadicOtherBuiltInRelationOpInvDyadic DyadicOtherBuiltInRelationOpInv
    deriving (Show)

data RelationExpCommalist = RelationExpCommalist RelationExp
                          | RelationExpCommalistCons RelationExpCommalist RelationExp
    deriving (Show)

data NadicUnion = NadicUnion RelationExpCommalist
                | NadicUnionHeaded Heading RelationExpCommalist
    deriving (Show)

data NadicDisjointUnion = NadicDisjointUnion RelationExpCommalist
                        | NadicDisjointUnionHeaded Heading RelationExpCommalist
    deriving (Show)

data NadicIntersect = NadicIntersect RelationExpCommalist
                    | NadicIntersectHeaded Heading RelationExpCommalist
    deriving (Show)

data NadicJoin = NadicJoin RelationExpCommalist
    deriving (Show)

data NadicTimes = NadicTimes RelationExpCommalist
    deriving (Show)

data NadicXunion = NadicXunion RelationExpCommalist
                 | NadicXunionHeaded Heading RelationExpCommalist
    deriving (Show)

data NadicCompose = NadicCompose RelationExpCommalist
    deriving (Show)

data NadicOtherBuiltInRelationOpInv = NadicOtherBuiltInRelationOpInvUnion NadicUnion
                                    | NadicOtherBuiltInRelationOpInvDisjointUnion NadicDisjointUnion
                                    | NadicOtherBuiltInRelationOpInvIntersect NadicIntersect
                                    | NadicOtherBuiltInRelationOpInvJoin NadicJoin
                                    | NadicOtherBuiltInRelationOpInvTimes NadicTimes
                                    | NadicOtherBuiltInRelationOpInvXunion NadicXunion
                                    | NadicOtherBuiltInRelationOpInvCompose NadicCompose
    deriving (Show)

data Project = Project RelationExp AttributeRefCommalist
             | ProjectAllBut RelationExp AttributeRefCommalist
    deriving (Show)

data THE_OpInv = THE_OpInv THE_OpName ScalarExp
    deriving (Show)

data THE_OpName = THE_OpName Identifier
    deriving (Show)

data TupleWrap = TupleWrap TupleExp Wrapping
    deriving (Show)

data TupleUnwrap = TupleUnwrap TupleExp Unwrapping
    deriving (Show)

data DyadicTupleUnion = DyadicTupleUnion TupleExp TupleExp
    deriving (Show)

data DyadicTupleCompose = DyadicTupleCompose TupleExp TupleExp
    deriving (Show)

data TupleRename = TupleRename TupleExp RenamingCommalist
    deriving (Show)

data TupleExtend = TupleExtend TupleExp AttributeAssignCommalist
    deriving (Show)

data MonadicOtherBuiltInTupleOpInv = MonadicOtherBuiltInTupleOpInvRename TupleRename
                                   | MonadicOtherBuiltInTupleOpInvExtend TupleExtend
                                   | MonadicOtherBuiltInTupleOpInvWrap TupleWrap
                                   | MonadicOtherBuiltInTupleOpInvUnwrap TupleUnwrap
    deriving (Show)

data NadicTupleUnion = NadicTupleUnion TupleExpCommalist
    deriving (Show)

data TupleProject = TupleProject TupleExp AttributeRefCommalist
                  | TupleProjectAllBut TupleExp AttributeRefCommalist
    deriving (Show)

data TupleComponent = TupleComponent AttributeName Exp
    deriving (Show)

data TupleComponentCommalist = TupleComponentCommalist TupleComponent
                             | TupleComponentCommalistCons TupleComponentCommalist TupleComponent
    deriving (Show)

data NadicOtherBuiltInTupleOpInv = NadicOtherBuiltInTupleOpInv NadicTupleUnion
    deriving (Show)

data TupleExtractorInv = TupleExtractorInv RelationExp
    deriving (Show)

data AttributeExtractorInv = AttributeExtractorInv AttributeRef TupleExp
    deriving (Show)

data TupleSelectorInv = TupleSelectorInv TupleComponentCommalist
    deriving (Show)

data TupleTypeName = TupleTypeName Heading
    deriving (Show)

data UserScalarTypeName = UserScalarTypeName Identifier
    deriving (Show)

data BuiltInScalarTypeName = BuiltInScalarTypeNameInteger
                           | BuiltInScalarTypeNameRational
                           | BuiltInScalarTypeNameCharacter
                           | BuiltInScalarTypeNameBoolean
    deriving (Show)

data TupleTypeSpec = TupleTypeSpecTupleTypeName TupleTypeName
                   | TupleTypeSpecSameTypeAs TupleExp
                   | TupleTypeSpecSameHeadingAs NonscalarExp
    deriving (Show)

data NonscalarTypeSpec = NonscalarTypeSpecTupleTypeSpec TupleTypeSpec
                       | NonscalarTypeSpecRelationTypeSpec RelationTypeSpec
    deriving (Show)

data MonadicOrDyadicOtherBuiltInTupleOpInv = MonadicOrDyadicOtherBuiltInTupleOpInvMonadic MonadicOtherBuiltInTupleOpInv
                                           | MonadicOrDyadicOtherBuiltInTupleOpInvDyadic DyadicOtherBuiltInTupleOpInv
    deriving (Show)

type Identifier = String

data PrivateOrPublic = PrivateOrPublicPrivate
                     | PrivateOrPublicPublic
    deriving (Show)

data ApplicationRelationVarDef = ApplicationRelationVarDef RelationVarName PrivateOrPublic RelationTypeOrInitValue KeyDefList
    deriving (Show)

data ArrayCardinality = ArrayCardinality ArrayVarRef
    deriving (Show)

data ArrayTarget = ArrayTarget ArrayVarRef
    deriving (Show)

data ArrayVarDef = ArrayVarDef ArrayVarName TupleTypeSpec
    deriving (Show)

data Assign = AssignScalar ScalarAssign
            | AssignNonscalar NonscalarAssign
    deriving (Show)

data ScalarAssign = ScalarAssignTarget ScalarTarget ScalarExp
                  | ScalarAssignUpdate ScalarUpdate
    deriving (Show)

data ScalarUpdate = ScalarUpdate ScalarTarget PossrepComponentAssignCommalist
    deriving (Show)

data ScalarTarget = ScalarTargetVarRef ScalarVarRef
                  | ScalarTargetTHE_PvRef ScalarTHE_PvRef
    deriving (Show)

data ScalarTHE_PvRef = ScalarTHE_PvRef THE_PvName ScalarTarget
    deriving (Show)

data NonscalarAssign = NonscalarAssignTuple TupleAssign
                     | NonscalarAssignRelation RelationAssign
    deriving (Show)

data TupleAssign = TupleAssignTarget TupleTarget TupleExp
                 | TupleAssignUpdate TupleUpdate
    deriving (Show)

data TupleTarget = TupleTargetVarRef TupleVarRef
                 | TupleTargetTHE_PvRef TupleTHE_PvRef
    deriving (Show)

data TupleUpdate = TupleUpdate TupleTarget AttributeAssignCommalist
    deriving (Show)

data TupleTHE_PvRef = TupleTHE_PvRef THE_PvName ScalarTarget
    deriving (Show)

data THE_PvName = THE_PvName String
    deriving (Show)

data RelationAssign = RelationAssignTarget RelationTarget RelationExp
                    | RelationAssignInsert RelationInsert
                    | RelationAssignDinsert RelationDinsert
                    | RelationAssignDelete RelationDelete
                    | RelationAssignIdelete RelationIdelete
                    | RelationAssignUpdate RelationUpdate
    deriving (Show)

data RelationInsert = RelationInsert RelationTarget RelationExp
    deriving (Show)

data RelationDinsert = RelationDinsert RelationTarget RelationExp
    deriving (Show)

data RelationDelete = RelationDelete RelationTarget RelationExp
                    | RelationDeleteWhere RelationTarget BoolExp
    deriving (Show)

data RelationIdelete = RelationIdelete RelationTarget RelationExp
    deriving (Show)

data RelationUpdate = RelationUpdate RelationTarget AttributeAssignCommalist
                    | RelationUpdateWhere RelationTarget BoolExp AttributeAssignCommalist
    deriving (Show)

data RelationTarget = RelationTargetVarRef RelationVarRef
                    | RelationTargetTHE_PvRef RelationTHE_PvRef
    deriving (Show)

data RelationTHE_PvRef = RelationTHE_PvRef THE_PvName ScalarTarget
    deriving (Show)

data PossrepComponentAssignCommalist = PossrepComponentAssignCommalist PossrepComponentAssign
                                     | PossrepComponentAssignCommalistCons PossrepComponentAssignCommalist PossrepComponentAssign
    deriving (Show)

data PossrepComponentAssign = PossrepComponentAssignFake String
    deriving (Show)

data Assignment = Assignment AssignCommalist Attribute
    deriving (Show)

data AssignCommalist = AssignCommalist Assign
                     | AssignCommalistCons AssignCommalist Assign
    deriving (Show)

data AttributeTarget = AttributeTargetRef AttributeRef
                     | AttributeTargetTHE_PvRef AttributeTHE_PvRef
    deriving (Show)

data AttributeTHE_PvRef = AttributeTHE_PvRef THE_PvName AttributeTarget
    deriving (Show)

data BeginTransaction = BeginTransaction
    deriving (Show)

data Call = Call UserOpInv
    deriving (Show)

}
