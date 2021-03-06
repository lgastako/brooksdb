{
module Language.Heidi.Parser ( parse ) where

import Prelude hiding (Ordering)

import Data.Char ( isSpace, isAlpha, isDigit )
import Language.Heidi.Lexer

}

%name parse
%tokentype { Token }
%error     { parseError }

%token
--      Keywords
        BEGIN           { BeginTok         }
        TRANSACTION     { TransactionTok   }
        CASE            { CaseTok          }
        END             { EndTok           }
        ELSE            { ElseTok          }
        CALL            { CallTok          }
        WHEN            { WhenTok          }
        THEN            { ThenTok          }
        IF              { IfTok            }
        DO              { DoTok            }
        TO              { ToTok            }
        WHILE           { WhileTok         }
        LEAVE           { LeaveTok         }
        CONSTRAINT      { ConstraintTok    }
        DROP            { DropTok          }
        VIRTUAL         { VirtualTok       }
        ASC             { AscTok           }
        DESC            { DescTok          }
        ORDINAL         { OrdinalTok       }
        ORDERED         { OrderedTok       }
        POSSREP         { PossrepTok       }
        RETURN          { ReturnTok        }
        RETURNS         { ReturnsTok       }
        TYPE            { TypeTok          }
        LOAD            { LoadTok          }
        ORDER           { OrderTok         }
        OPERATOR        { OperatorTok      }
        UPDATES         { UpdatesTok       }
        COMMIT          { CommitTok        }
        ROLLBACK        { RollbackTok      }
        VAR             { VarTok           }
        INIT            { InitTok          }
        TUPLE           { TupleTok         }
        FROM            { FromTok          }
        ALL             { AllTok           }
        BUT             { ButTok           }
        UNION           { UnionTok         }
        RENAME          { RenameTok        }
        EXTEND          { ExtendTok        }
        WRAP            { WrapTok          }
        UNWRAP          { UnwrapTok        }
        AS              { AsTok            }
        COMPOSE         { ComposeTok       }
        HEADING         { HeadingTok       }
        D_UNION         { DunionTok        }
        INTERSECT       { IntersectTok     }
        JOIN            { JoinTok          }
        TIMES           { TimesTok         }
        XUNION          { XunionTok        }
        where           { WhereTok         }
        GROUP           { GroupTok         }
        UNGROUP         { UngroupTok       }
        TCLOSE          { TcloseTok        }
        MINUS           { MinusTok         }
        I_MINUS         { IminusTok        }
        NOT             { NotTok           }
        MATCHING        { MatchingTok      }
        DIVIDEBY        { DividebyTok      }
        SUMMARIZE       { SummarizeTok     }
        PER             { PerTok           }
        BY              { ByTok            }
        PREFIX          { PrefixTok        }
        SUFFIX          { SuffixTok        }
        KEY             { KeyTok           }
        INTEGER         { IntegerTok       }
        RATIONAL        { RationalTok      }
        CHARACTER       { CharacterTok     }
        BOOLEAN         { BooleanTok       }
        REAL            { RealTok          }
        BASE            { BaseTok          }
        RELATION        { RelationTok      }
        WITH            { WithTok          }
        TABLE_DEE       { TableDeeTok      }
        TABLE_DUM       { TableDumTok      }
        PRIVATE         { PrivateTok       }
        PUBLIC          { PublicTok        }
        COUNT           { CountTok         }
        ARRAY           { ArrayTok         }
        UPDATE          { UpdateTok        }
        INSERT          { InsertTok        }
        DELETE          { DeleteTok        }
        I_DELETE        { IdeleteTok       }
        D_INSERT        { DinsertTok       }
        SAME_TYPE_AS    { SameTypeAsTok    }
        SAME_HEADING_AS { SameHeadingAsTok }
        COUNTD          { CountdTok        }
        SUM             { SumTok           }
        SUMD            { SumdTok          }
        AVG             { AvgTok           }
        AVGD            { AvgdTok          }
        MAX             { MaxTok           }
        MIN             { MinTok           }
        AND             { AndTok           }
        OR              { OrTok            }
        XOR             { XorTok           }
        EXACTLY         { ExactlyTok       }
        EXACTLYD        { ExactlydTok      }
        TRUE            { TrueTok          }
        FALSE           { FalseTok         }
        '('             { LeftRoundTok     }
        ')'             { RightRoundTok    }
        '{'             { LeftCurlyTok     }
        '}'             { RightCurlyTok    }
        ':'             { ColonTok         }
        ';'             { SemiColonTok     }
        ','             { CommaTok         }
        ':='            { AssignerTok      }
        '='             { EqualTok         }
        '<>'            { NotEqualTok      }

--      Parameterized tokens
        varName         { IdentTok $$      }
        int             { IntTok $$        }
        strLiteral      { StrTok $$        }

%%

Statement : StatementBody ';'                                       { Statement $1 }

RealRelationVarDef : VAR RelationVarName RealOrBase
                         RelationTypeOrInitValue KeyDefList         { RealRelationVarDef $2 $3 $4 $5 }

ApplicationRelationVarDef : VAR RelationVarName PrivateOrPublic
                                RelationTypeOrInitValue KeyDefList  { ApplicationRelationVarDef $2 $3 $4 $5 }

PrivateOrPublic: PRIVATE                                            { PrivateOrPublicPrivate }
               | PUBLIC                                             { PrivateOrPublicPublic }

KeyDefList : KeyDef                                                 { KeyDefList $1 }
           | KeyDefList ',' KeyDef                                  { KeyDefListCons $1 $3 }

KeyDef : KEY '{' AttributeRefCommalist '}'                          { KeyDef $3 }
       | KEY '{' ALL BUT AttributeRefCommalist '}'                  { KeyDefAllBut $5 }

RealOrBase : REAL                                                   { RealOrBaseReal }
           | BASE                                                   { RealOrBaseBase }

RelationVarName : varName                                           { RelationVarName $1 }

RelationTypeOrInitValue : RelationTypeSpec                          { RelationTypeOrInitValueRelationTypeSpec $1        }
                        | INIT '(' RelationExp ')'                  { RelationTypeOrInitValueInit $3                    }
                        | RelationTypeSpec INIT '(' RelationExp ')' { RelationTypeOrInitValueRelationTypeSpecInit $1 $4 }

RelationTypeSpec : RelationTypeName                                 { RelationTypeSpecRelationTypeName $1 }
                 | SAME_TYPE_AS '(' RelationExp ')'                 { RelationTypeSpecSameTypeAs $3       }
                 | RELATION SAME_HEADING_AS '(' NonscalarExp ')'    { RelationTypeSpecSameHeadingAs $4    }

RelationTypeName : RELATION Heading                                 { RelationTypeName $2 }

Heading : '{' AttributeCommalist '}'                                { Heading $2 }

AttributeCommalist : Attribute                                      { AttributeCommalist $1 }
                   | AttributeCommalist ',' Attribute               { AttributeCommalistCons $1 $3 }

Attribute : AttributeName TypeSpec                                  { Attribute $1 $2 }

TypeSpec : ScalarTypeSpec                                           { TypeSpecScalar $1 }
         | NonscalarTypeSpec                                        { TypeSpecNonscalar $1 }

ScalarTypeSpec : ScalarTypeName                                     { ScalarTypeSpecScalarTypeName $1 }
               | SAME_TYPE_AS '(' ScalarExp ')'                     { ScalarTypeSpecSameTypeAs $3 }

ScalarTypeName : UserScalarTypeName                                 { ScalarTypeNameUser $1 }
               | BuiltInScalarTypeName                              { ScalarTypeNameBuiltIn $1 }

UserScalarTypeName : varName                                        { UserScalarTypeName $1 }

BuiltInScalarTypeName : INTEGER                                     { BuiltInScalarTypeNameInteger }
                      | RATIONAL                                    { BuiltInScalarTypeNameRational }
                      | CHARACTER                                   { BuiltInScalarTypeNameCharacter }
                      | BOOLEAN                                     { BuiltInScalarTypeNameBoolean }

NonscalarTypeSpec : TupleTypeSpec                                   { NonscalarTypeSpecTupleTypeSpec $1 }
                  | RelationTypeSpec                                { NonscalarTypeSpecRelationTypeSpec $1 }

TupleTypeSpec : TupleTypeName                                       { TupleTypeSpecTupleTypeName $1 }
              | SAME_TYPE_AS '(' TupleExp ')'                       { TupleTypeSpecSameTypeAs $3 }
              | TUPLE SAME_HEADING_AS '(' NonscalarExp ')'          { TupleTypeSpecSameHeadingAs $4 }

TupleTypeName : TUPLE Heading                                       { TupleTypeName $2 }

NonscalarExp : TupleExp                                             { NonscalarExpTupleExp $1 }
             | RelationExp                                          { NonscalarExpRelationExp $1 }

TupleExp : TupleWithExp                                             { TupleExpWith $1 }
         | TupleNonwithExp                                          { TupleExpNonwith $1 }

TupleWithExp : WITH '(' NameIntroCommalist ')' ':' TupleExp         { TupleWithExp $3 $6 }

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

TupleSelectorInv : TUPLE '{' TupleComponentCommalist '}'            { TupleSelectorInv $3 }

TupleComponentCommalist : TupleComponent                             { TupleComponentCommalist $1 }
                        | TupleComponentCommalist ',' TupleComponent { TupleComponentCommalistCons $1 $3 }

TupleComponent : AttributeName Exp                                  { TupleComponent $1 $2 }

AttributeExtractorInv : AttributeRef FROM TupleExp                  { AttributeExtractorInv $1 $3 }

AttributeRef : AttributeName                                        { AttributeRef $1 }

AttributeRefCommalist : AttributeRef                                { AttributeRefCommalist $1 }
                      | AttributeRefCommalist ',' AttributeRef      { AttributeRefCommalistCons $1 $3 }

AttributeName : varName                                             { AttributeName $1 }

TupleExtractorInv : TUPLE FROM RelationExp                          { TupleExtractorInv $3 }

TupleProject : TupleExp '{' AttributeRefCommalist '}'               { TupleProject $1 $3 }
             | TupleExp '{' ALL BUT AttributeRefCommalist '}'       { TupleProjectAllBut $1 $5 }

NadicOtherBuiltInTupleOpInv : NadicTupleUnion                       { NadicOtherBuiltInTupleOpInv $1 }

NadicTupleUnion : UNION '{' TupleExpCommalist '}'                   { NadicTupleUnion $3 }

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

DyadicTupleUnion : TupleExp UNION TupleExp                          { DyadicTupleUnion $1 $3 }

DyadicTupleCompose : TupleExp COMPOSE TupleExp                      { DyadicTupleCompose $1 $3 }

TupleRename : TupleExp RENAME '{' RenamingCommalist '}'             { TupleRename $1 $4 }

TupleExtend : EXTEND TupleExp ':' '{' AttributeAssignCommalist '}'  { TupleExtend $2 $5 }

RenamingCommalist : Renaming                                        { RenamingCommalist $1 }
                  | RenamingCommalist ',' Renaming                  { RenamingCommalistCons $1 $3 }

Renaming : AttributeRef AS IntroducedName                           { Renaming $1 $3 }
         | PREFIX CharacterStringLiteral
               AS CharacterStringLiteral                            { RenamingPrefix $2 $4 }
         | SUFFIX CharacterStringLiteral
               AS CharacterStringLiteral                            { RenamingSuffix $2 $4 }

CharacterStringLiteral : strLiteral                                 { CharacterStringLiteral $1 }

AttributeAssignCommalist : AttributeAssign                              { AttributeAssignCommalist $1 }
                         | AttributeAssignCommalist ',' AttributeAssign { AttributeAssignCommalistCons $1 $3 }

-- Inferred from TTMHC's <extend add>
-- TODO: Looks like the online stuff and book are not in sync.  Sync up I guess.  Hopefully pretty close.
AttributeAssign : Exp AS IntroducedName                             { AttributeAssign $1 $3 }

TupleWrap : TupleExp WRAP '(' Wrapping ')'                          { TupleWrap $1 $4 }

TupleUnwrap : TupleExp UNWRAP '(' Unwrapping ')'                    { TupleUnwrap $1 $4 }

Wrapping : '{' AttributeRefCommalist '}' AS IntroducedName          { Wrapping $2 $5 }
         | '{' ALL BUT AttributeRefCommalist '}' AS IntroducedName  { WrappingAllBut $4 $7 }

IntroducedName : varName                                            { IntroducedName $1 }

Unwrapping : AttributeRef                                           { Unwrapping $1 }

Subscript : IntegerExp                                              { Subscript $1 }

IntegerExp : int                                                    { IntegerExp $1 }

RelationExp : RelationWithExp                                       { RelationExpWith $1 }
            | RelationNonwithExp                                    { RelationExpNonwith $1 }

RelationWithExp : WITH '(' NameIntroCommalist ')' ':' RelationExp   { RelationWithExp $3 $6 }

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
        | RelationExp '{' ALL BUT AttributeRefCommalist '}'         { ProjectAllBut $1 $5 }

NadicOtherBuiltInRelationOpInv : NadicUnion                         { NadicOtherBuiltInRelationOpInvUnion $1         }
                               | NadicDisjointUnion                 { NadicOtherBuiltInRelationOpInvDisjointUnion $1 }
                               | NadicIntersect                     { NadicOtherBuiltInRelationOpInvIntersect $1     }
                               | NadicJoin                          { NadicOtherBuiltInRelationOpInvJoin $1          }
                               | NadicTimes                         { NadicOtherBuiltInRelationOpInvTimes $1         }
                               | NadicXunion                        { NadicOtherBuiltInRelationOpInvXunion $1        }
                               | NadicCompose                       { NadicOtherBuiltInRelationOpInvCompose $1       }

NadicUnion : UNION '{' RelationExpCommalist '}'                     { NadicUnion $3          }
           | UNION Heading '{' RelationExpCommalist '}'             { NadicUnionHeaded $2 $4 }

NadicDisjointUnion : D_UNION '{' RelationExpCommalist '}'           { NadicDisjointUnion $3          }
                   | D_UNION Heading '{' RelationExpCommalist '}'   { NadicDisjointUnionHeaded $2 $4 }

NadicIntersect : INTERSECT '{' RelationExpCommalist '}'             { NadicIntersect $3          }
               | INTERSECT Heading '{' RelationExpCommalist '}'     { NadicIntersectHeaded $2 $4 }

NadicJoin : JOIN '{' RelationExpCommalist '}'                       { NadicJoin $3 }

NadicTimes : JOIN '{' RelationExpCommalist '}'                      { NadicTimes $3 }

NadicXunion : XUNION '{' RelationExpCommalist '}'                   { NadicXunion $3          }
            | XUNION Heading '{' RelationExpCommalist '}'           { NadicXunionHeaded $2 $4 }

NadicCompose : JOIN '{' RelationExpCommalist '}'                    { NadicCompose $3 }

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

Rename : RelationExp RENAME '{' RenamingCommalist '}'               { Rename $1 $4  }
Where : RelationExp where BoolExp                                   { Where $1 $3   }
Extend : EXTEND RelationExp ':' '{' AttributeAssignCommalist '}'    { Extend $2 $5  }
Wrap : RelationExp WRAP '(' Wrapping ')'                            { Wrap $1 $4    }
Unwrap : RelationExp UNWRAP '(' Unwrapping ')'                      { Unwrap $1 $4  }
Group : RelationExp GROUP '(' Grouping ')'                          { Group $1 $4   }
Ungroup : RelationExp UNGROUP '(' Ungrouping ')'                    { Ungroup $1 $4 }
Tclose : TCLOSE '(' RelationExp ')'                                 { Tclose $3     }

-- JUST FOR NOW.
BoolExp : TRUE                                                      { BoolExpTrue  }
        | FALSE                                                     { BoolExpFalse }

Grouping : '{' AttributeRefCommalist '}'                            { Grouping $2       }
         | '{' ALL BUT AttributeRefCommalist '}'                    { GroupingAllBut $4 }

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

DyadicUnion : RelationExp UNION RelationExp                         { DyadicUnion $1 $3         }
DyadicDisjointUnion : RelationExp D_UNION RelationExp               { DyadicDisjointUnion $1 $3 }
DyadicIntersect : RelationExp INTERSECT RelationExp                 { DyadicIntersect $1 $3     }
Minus : RelationExp MINUS RelationExp                               { Minus $1 $3               }
IncludedMinus : RelationExp I_MINUS RelationExp                     { IncludedMinus $1 $3       }
DyadicJoin : RelationExp JOIN RelationExp                           { DyadicJoin $1 $3          }
DyadicTimes : RelationExp TIMES RelationExp                         { DyadicTimes $1 $3         }
DyadicXunion : RelationExp XUNION RelationExp                       { DyadicXunion $1 $3        }
DyadicCompose : RelationExp COMPOSE RelationExp                     { DyadicCompose $1 $3       }
Matching : RelationExp MATCHING RelationExp                         { Matching $1 $3            }
NotMatching : RelationExp NOT MATCHING RelationExp                  { NotMatching $1 $4         }
Divide : RelationExp DIVIDEBY RelationExp Per                       { Divide $1 $3 $4           }

Summarize : SUMMARIZE RelationExp ':' '{' AttributeAssignCommalist '}'           { Summarize $2 $5           }
          | SUMMARIZE RelationExp PerOrBy ':' '{' AttributeAssignCommalist '}'   { SummarizePerOrBy $2 $3 $6 }

Per : PER '(' RelationExp ')'                                       { Per $3 }
    --| PER '(' RelationExp ... many

PerOrBy : Per                                                       { PerOrByPer $1 }
        | By                                                        { PerOrByBy $1  }

-- Note: I inserted By as it's own element, the original grammar had it nested underPerOrBy.
By : BY '{' AttributeRefCommalist '}'                               { By $3 }
   | BY '{' ALL BUT AttributeRefCommalist '}'                       { ByAllBut $5 }

ScalarExp : ScalarWithExp                                           { ScalarExpWith $1    }
          | ScalarNonwithExp                                        { ScalarExpNonwith $1 }

ScalarWithExp : WITH '(' NameIntroCommalist ')' ':' ScalarExp       { ScalarWithExp $3 $6 }

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

RelationSelectorInv : RELATION '{' TupleExpCommalist '}'            { RelationSelectorInv $3 }
                    | RELATION Heading '{' TupleExpCommalist '}'    { RelationSelectorInvHeaded $2 $4 }
                    | TABLE_DEE                                     { RelationSelectorInvTableDee }
                    | TABLE_DUM                                     { RelationSelectorInvTableDum }

ArrayCardinality : COUNT '(' ArrayVarRef ')'                        { ArrayCardinality $3 }

ArrayTarget : ArrayVarRef                                           { ArrayTarget $1 }

ArrayVarDef : VAR ArrayVarName ARRAY TupleTypeSpec                  { ArrayVarDef $2 $4 }

Assign : ScalarAssign                                               { AssignScalar $1 }
       | NonscalarAssign                                            { AssignNonscalar $1 }

ScalarAssign : ScalarTarget ':=' ScalarExp                          { ScalarAssignTarget $1 $3 }
             | ScalarUpdate                                         { ScalarAssignUpdate $1 }

ScalarUpdate : UPDATE ScalarTarget
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

TupleUpdate : UPDATE TupleTarget '{' AttributeAssignCommalist '}'   { TupleUpdate $2 $4 }

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

RelationInsert : INSERT RelationTarget RelationExp                  { RelationInsert $2 $3 }

RelationDinsert : D_INSERT RelationTarget RelationExp               { RelationDinsert $2 $3 }

RelationDelete : DELETE RelationTarget RelationExp                  { RelationDelete $2 $3 }
               | DELETE RelationTarget where BoolExp                { RelationDeleteWhere $2 $4 }

RelationIdelete : I_DELETE RelationTarget RelationExp               { RelationIdelete $2 $3 }

RelationUpdate : UPDATE RelationTarget
                        '{' AttributeAssignCommalist '}'            { RelationUpdate $2 $4 }
               | UPDATE RelationTarget where BoolExp
                        '{' AttributeAssignCommalist '}'            { RelationUpdateWhere $2 $4 $6 }

Assignment : AssignCommalist Attribute                              { Assignment $1 $2 }

AssignCommalist : Assign                                            { AssignCommalist $1 }
                | AssignCommalist ',' Assign                        { AssignCommalistCons $1 $3 }

AttributeTarget : AttributeRef                                      { AttributeTargetRef $1 }
                | AttributeTHE_PvRef                                { AttributeTargetTHE_PvRef $1 }

AttributeTHE_PvRef : THE_PvName '(' AttributeTarget ')'             { AttributeTHE_PvRef $1 $3 }

BeginTransaction : BEGIN TRANSACTION                                { BeginTransaction }

Call : CALL UserOpInv                                               { Call $2 }

Case : CASE ';' WhenSpecList END CASE                               { Case $3 }
     | CASE ';' WhenSpecList ELSE Statement END CASE                { CaseElse $3 $5 }

WhenSpecList : WhenSpec                                             { WhenSpecList $1 }
             | WhenSpecList ',' WhenSpec                            { WhenSpecListCons $1 $3 }

WhenSpec : WHEN BoolExp THEN Statement                              { WhenSpec $2 $4 }

StatementBody : WithStatementBody                                   { StatementBodyWith $1 }
              | NonwithStatementBody                                { StatementBodyNonwith $1 }

WithStatementBody : WITH '(' NameIntroCommalist ')'
                         ':' StatementBody                          { WithStatementBody $3 $6 }

NonwithStatementBody
-- No def, no idea, so omitting for now.
--   : PreviouslyDefinedStatementBodyCommalist      { NonwithStatementBodyPreviouslyDSBC $1 }
                     : BeginTransaction                             { NonwithStatementBodyBeginTransaction $1 }
                     | Commit                                       { NonwithStatementBodyCommit $1 }
                     | Rollback                                     { NonwithStatementBodyRollback $1 }
                     | Call                                         { NonwithStatementBodyCall $1 }
                     | Return                                       { NonwithStatementBodyReturn $1 }
                     | Case                                         { NonwithStatementBodyCase $1 }
                     | If                                           { NonwithStatementBodyIf $1 }
                     | Do                                           { NonwithStatementBodyDo $1 }
                     | While                                        { NonwithStatementBodyWhile $1 }
                     | Leave                                        { NonwithStatementBodyLeave $1 }
                     | CompoundStatementBody                        { NonwithStatementBodyCompoundStatementBody $1 }
                     -- | Noop                                         { NonwithStatementBodyNoop $1 }
                     -- Don't want to think through interactions with rest of whitespace, so for now, omitted.

Commit : COMMIT                                                     { Commit }

Rollback : ROLLBACK                                                 { Rollback }

Return : RETURN                                                     { Return }
       | RETURN Exp                                                 { ReturnExp $2 }

If : IF BoolExp THEN Statement END IF                               { If $2 $4 }
   | IF BoolExp THEN Statement ELSE Statement END IF                { IfElse $2 $4 $6 }

Do : DO ScalarVarRef ':=' IntegerExp TO IntegerExp Statement END DO { Do $2 $4 $6 $7 }
   | StatementName ':' DO
        ScalarVarRef ':=' IntegerExp TO IntegerExp Statement END DO { DoNamed $1 $4 $6 $8 $9 }

StatementName : varName                                             { StatementName $1 }

CompoundStatementBody : BEGIN ';' StatementList END                 { CompoundStatementBody $3 }

StatementList : Statement                                           { StatementList $1 }
              | StatementList ';' Statement                         { StatementListCons $1 $3 }

While : WHILE BoolExp ';' Statement END WHILE                       { While $2 $4 }
      | StatementName ':' WHILE BoolExp ';' Statement END WHILE     { WhileNamed $1 $4 $6 }

Leave : LEAVE StatementName                                         { Leave $2 }

ConstraintDef : CONSTRAINT ConstraintName BoolExp                   { ConstraintDef $2 $3 }

ConstraintDrop : DROP CONSTRAINT ConstraintName                     { ConstraintDrop $3 }

ConstraintName : varName                                            { ConstraintName $1 }

DatabaseRelationVarDef : RealRelationVarDef                         { DatabaseRelationVarDefReal $1 }
                       | VirtualRelationVarDef                      { DatabaseRelationVarDefVirtual $1 }

VirtualRelationVarDef : VAR RelationVarName
                            VIRTUAL '(' RelationExp ')' KeyDefList  { VirtualRelationVarDef $2 $5 $7 }

Direction : ASC                                                     { DirectionAsc }
          | DESC                                                    { DirectionDesc }


NonscalarSelectorInv : TupleSelectorInv                             { NonscalarSelectorInvTuple $1 }
                     | RelationSelectorInv                          { NonscalarSelectorInvRelation $1 }

NonscalarVarRef : TupleVarRef                                       { NonscalarVarRefTuple $1 }
                | RelationVarRef                                    { NonscalarVarRefRelation $1 }

OrderItem : Direction AttributeRef                                  { OrderItem $1 $2 }

OrderItemCommalist : OrderItem                                      { OrderItemCommalist $1 }
                   | OrderItemCommalist ',' OrderItem               { OrderItemCommalistCons $1 $3 }

Ordering : ORDINAL                                                  { OrderingOrdinal }
         | ORDERED                                                  { OrderingOrdered }

ParameterDef : ParameterName TypeSpec                               { ParameterDef $1 $2 }

ParameterDefCommalist : ParameterDef                                { ParameterDefCommalist $1 }
                      | ParameterDefCommalist ',' ParameterDef      { ParameterDefCommalistCons $1 $3 }

ParameterName : varName                                             { ParameterName $1 }

ParameterNameCommalist : ParameterName                              { ParameterNameCommalist $1 }
                       | ParameterNameCommalist ',' ParameterName   { ParameterNameCommalistCons $1 $3 }

PossrepComponentDef : PossrepComponentName TypeSpec                 { PossrepComponentDef $1 $2 }

PossrepComponentRef : PossrepComponentName                          { PossrepComponentRef $1 }

PossrepComponentName : varName                                      { PossrepComponentName $1 }

PossrepTHE_PvRef : THE_PvName '(' PossrepComponentTarget ')'        { PossrepTHE_PvRef $1 $3 }

PossrepComponentTarget : PossrepComponentRef                        { PossrepComponentTargetRef $1 }
                       | PossrepTHE_PvRef                           { PossrepComponentTargetTHE_PvRef $1 }

PossrepConstraintDef : CONSTRAINT BoolExp                           { PossrepConstraintDef $2 }

PossrepDef : POSSREP '{' PossrepComponentDefCommalist '}'           { PossrepDef $3 }
           | POSSREP PossrepName
                     '{' PossrepComponentDefCommalist '}'           { PossrepDefNamed $2 $4}
           | POSSREP '{' PossrepComponentDefCommalist
                         PossrepConstraintDef         '}'           { PossrepDefConstrained $3 $4 }
           | POSSREP PossrepName
                     '{' PossrepComponentDefCommalist
                         PossrepConstraintDef         '}'           { PossrepDefNamedConstrained $2 $4 $5 }

PossrepDefList : PossrepDef                                         { PossrepDefList $1 }
               | PossrepDefList ';' PossrepDef                      { PossrepDefListCons $1 $3 }

PossrepComponentDefCommalist : PossrepComponentDef                  { PossrepComponentDefCommalist $1 }
                             | PossrepComponentDefCommalist ','
                               PossrepComponentDef                  { PossrepComponentDefCommalistCons $1 $3 }

PreviouslyDefinedStatementBody : Assignment                         { PDSBAssignment $1 }
                               | UserOpDef                          { PDSBUserOpDef $1 }
                               | UserOpDrop                         { PDSBUserOpDrop $1 }
                               | UserScalarTypeDef                  { PDSBUserScalarTypeDef $1 }
                               | UserScalarTypeDrop                 { PDSBUserScalarTypeDrop $1 }
                               | ScalarVarDef                       { PDSBScalarVarDef $1 }
                               | TupleVarDef                        { PDSBTupleVarDef $1 }
                               | RelationVarDef                     { PDSBRelationVarDef $1 }
                               | RelationVarDrop                    { PDSBRelationVarDrop $1 }
                               | ConstraintDef                      { PDSBConstraintDef $1 }
                               | ConstraintDrop                     { PDSBConstraintDrop $1 }
                               | ArrayVarDef                        { PDSBArrayVarDef $1 }
                               | RelationGet                        { PDSBRelationGet $1 }
                               | RelationSet                        { PDSBRelationSet $1 }

UserOpDef : UserUpdateOpDef                                         { UserOpDefUpdate $1 }
          | UserReadOnlyOpDef                                       { UserOpDefReadOnly $1 }

UserUpdateOpDef : OPERATOR UserOpName '(' ParameterDefCommalist ')'
                           UPDATES '{' ParameterNameCommalist '}' ';'
                           Statement END OPERATOR                   { UserUpdateOpDef $2 $4 $8 $11 }
                | OPERATOR UserOpName '(' ParameterDefCommalist ')'
                           UPDATES '{' ALL BUT ParameterNameCommalist '}' ';'
                           Statement END OPERATOR                   { UserUpdateOpDefAllBut $2 $4 $10 $13 }

UserReadOnlyOpDef : OPERATOR UserOpName '(' ParameterDefCommalist ')'
                             RETURNS TypeSpec ';' Statement
                             END OPERATOR                           { UserReadOnlyOpDef $2 $4 $7 $9 }

UserOpDrop : DROP OPERATOR UserOpName                               { UserOpDrop $3 }

UserScalarTypeDef : UserScalarRootTypeDef                           { UserScalarTypeDef $1 }

UserScalarRootTypeDef : TYPE UserScalarTypeName PossrepDefList
                             INIT '(' Literal ')'                   { UserScalarRootTypeDef $2 $3 $6 }
                      | TYPE UserScalarTypeName Ordering
                             PossrepDefList INIT '(' Literal ')'    { UserScalarRootTypeDefOrdered $2 $3 $4 $7 }

-- eh? undefined
Literal : varName                                                   { Literal $1 }

UserScalarTypeDrop : DROP TYPE UserScalarTypeName                   { UserScalarTypeDrop $3 }

ScalarVarDef : VAR ScalarVarName ScalarTypeOrInitValue              { ScalarVarDef $2 $3 }

TupleVarDef : VAR TupleVarName TupleTypeOrInitValue                 { TupleVarDef $2 $3 }

TupleTypeOrInitValue : TupleTypeSpec                                { TupleTypeOrInitValueTupleTypeSpec $1 }
                     | INIT '(' TupleExp ')'                        { TupleTypeOrInitValueInit $3 }
                     | TupleTypeSpec INIT '(' TupleExp ')'          { TupleTypeOrInitValueTupleTypeSpecInit $1 $4 }

RelationVarDef : DatabaseRelationVarDef                             { RelationVarDefDatabase $1 }
               | ApplicationRelationVarDef                          { RelationVarDefApplication $1 }

RelationVarDrop : DROP VAR RelationVarRef                           { RelationVarDrop $3 }

RelationGet : LOAD ArrayTarget
              FROM RelationExp
              ORDER '(' OrderItemCommalist ')'                      { RelationGet $2 $4 $7 }

RelationSet : LOAD RelationTarget FROM ArrayVarRef                  { RelationSet $2 $4 }

ScalarTypeOrInitValue : ScalarTypeSpec                              { ScalarTypeOrInitValueScalarTypeSpec $1 }
                      | INIT '(' ScalarExp ')'                      { ScalarTypeOrInitValueInit $3 }
                      | ScalarTypeSpec INIT '(' ScalarExp ')'       { ScalarTypeOrInitValueScalarTypeSpecInit $1 $4 }

RelationComp : RelationExp RelationCompOp RelationExp               { RelationComp $1 $2 $3 }

RelationCompOp : '='                                                { RelationCompOpEq }
               | '<>'                                               { RelationCompOpNe }
               -- others, but we'll get to them.

ScalarComp : ScalarExp ScalarCompOp ScalarExp                       { ScalarComp $1 $2 $3 }

ScalarCompOp : '='                                                  { ScalarCompOpEq }
             | '<>'                                                 { ScalarCompOpNe }

SelectorInv : ScalarSelectorInv                                     { SelectorInvScalar $1 }
            | NonscalarSelectorInv                                  { SelectorInvNonscalar $1 }

Summary : SummarySpec '(' ')'                                       { Summary $1 }
        | SummarySpec '(' Exp ')'                                   { SummaryExp $1 $3 }
        | SummarySpec '(' IntegerExp ',' Exp ')'                    { SummaryExpIntegerExpExp $1 $3 $5 }

SummarySpec : COUNT                                                 { SummarySpecCount     }
            | COUNTD                                                { SummarySpecCountd    }
            | SUM                                                   { SummarySpecSum       }
            | SUMD                                                  { SummarySpecSumd      }
            | AVG                                                   { SummarySpecAvg       }
            | AVGD                                                  { SummarySpecAvgd      }
            | MAX                                                   { SummarySpecMax       }
            | MIN                                                   { SummarySpecMin       }
            | AND                                                   { SummarySpecAnd       }
            | OR                                                    { SummarySpecOr        }
            | XOR                                                   { SummarySpecXor       }
            | EXACTLY                                               { SummarySpecExactly   }
            | EXACTLYD                                              { SummarySpecExactlyd  }
            | UNION                                                 { SummarySpecUnion     }
            | D_UNION                                               { SummarySpecDunion    }
            | INTERSECT                                             { SummarySpecIntersect }
            | XUNION                                                { SummarySpecXunion    }

TupleComp : TupleExp TupleCompOp TupleExp                           { TupleComp $1 $2 $3 }
               -- TODO: add the other two later.

TupleCompOp : '='                                                   { TupleCompOpEq }
            | '<>'                                                  { TupleCompOpNe }

VarRef : ScalarVarRef                                               { VarRefScalar $1 }
       | NonscalarVarRef                                            { VarRefNonscalar $1 }

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

data THE_PvName = THE_PvName Identifier
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

data PossrepComponentAssign = PossrepComponentAssignFake Identifier
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

data Case = Case WhenSpecList
          | CaseElse WhenSpecList Statement
    deriving (Show)

data Do = Do ScalarVarRef IntegerExp IntegerExp Statement
        | DoNamed StatementName ScalarVarRef IntegerExp IntegerExp Statement
    deriving (Show)

data Leave = Leave StatementName
    deriving (Show)

data WhenSpecList = WhenSpecList WhenSpec
                  | WhenSpecListCons WhenSpecList WhenSpec
    deriving (Show)

data WhenSpec = WhenSpec BoolExp Statement
    deriving (Show)

data Statement = Statement StatementBody
    deriving (Show)

data StatementBody = StatementBodyWith WithStatementBody
                   | StatementBodyNonwith NonwithStatementBody
    deriving (Show)

data WithStatementBody = WithStatementBody NameIntroCommalist StatementBody
    deriving (Show)

data NonwithStatementBody = NonwithStatementBodyBeginTransaction BeginTransaction
                          | NonwithStatementBodyCommit Commit
                          | NonwithStatementBodyRollback Rollback
                          | NonwithStatementBodyCall Call
                          | NonwithStatementBodyReturn Return
                          | NonwithStatementBodyCase Case
                          | NonwithStatementBodyIf If
                          | NonwithStatementBodyDo Do
                          | NonwithStatementBodyWhile While
                          | NonwithStatementBodyLeave Leave
                          | NonwithStatementBodyCompoundStatementBody CompoundStatementBody
    deriving (Show)

data Commit = Commit
    deriving (Show)

data Rollback = Rollback
    deriving (Show)

data CompoundStatementBody = CompoundStatementBody StatementList
    deriving (Show)

data StatementList = StatementList Statement
                   | StatementListCons StatementList Statement
    deriving (Show)

data While = While BoolExp Statement
           | WhileNamed StatementName BoolExp Statement
    deriving (Show)

data StatementName = StatementName Identifier
    deriving (Show)

data If = If BoolExp Statement
        | IfElse BoolExp Statement Statement
    deriving (Show)

data Return = Return
            | ReturnExp Exp
    deriving (Show)

data ConstraintDef = ConstraintDef ConstraintName BoolExp
    deriving (Show)

data ConstraintName = ConstraintName Identifier
    deriving (Show)

data ConstraintDrop = ConstraintDrop ConstraintName
    deriving (Show)

data DatabaseRelationVarDef = DatabaseRelationVarDefReal RealRelationVarDef
                            | DatabaseRelationVarDefVirtual VirtualRelationVarDef
    deriving (Show)

data VirtualRelationVarDef = VirtualRelationVarDef RelationVarName RelationExp KeyDefList
    deriving (Show)

data Direction = DirectionAsc
               | DirectionDesc
    deriving (Show)

data NonscalarSelectorInv = NonscalarSelectorInvTuple TupleSelectorInv
                          | NonscalarSelectorInvRelation RelationSelectorInv
    deriving (Show)

data NonscalarVarRef = NonscalarVarRefTuple TupleVarRef
                     | NonscalarVarRefRelation RelationVarRef
    deriving (Show)

data OrderItem = OrderItem Direction AttributeRef
    deriving (Show)

data Ordering = OrderingOrdinal
              | OrderingOrdered
    deriving (Show)

data ParameterDef = ParameterDef ParameterName TypeSpec
    deriving (Show)

data ParameterName = ParameterName Identifier
    deriving (Show)

data ParameterNameCommalist = ParameterNameCommalist ParameterName
                            | ParameterNameCommalistCons ParameterNameCommalist ParameterName
    deriving (Show)

data PossrepComponentDef = PossrepComponentDef PossrepComponentName TypeSpec
    deriving (Show)

data PossrepComponentRef = PossrepComponentRef PossrepComponentName
    deriving (Show)

data PossrepComponentName = PossrepComponentName Identifier
    deriving (Show)

data PossrepComponentTarget = PossrepComponentTargetRef PossrepComponentRef
                            | PossrepComponentTargetTHE_PvRef PossrepTHE_PvRef
    deriving (Show)

data PossrepTHE_PvRef = PossrepTHE_PvRef THE_PvName PossrepComponentTarget
    deriving (Show)

data PossrepConstraintDef = PossrepConstraintDef BoolExp
    deriving (Show)

data PossrepDef = PossrepDef PossrepComponentDefCommalist
                | PossrepDefNamed PossrepName PossrepComponentDefCommalist
                | PossrepDefConstrained PossrepComponentDefCommalist PossrepConstraintDef
                | PossrepDefNamedConstrained PossrepName PossrepComponentDefCommalist PossrepConstraintDef
    deriving (Show)

data PossrepComponentDefCommalist = PossrepComponentDefCommalist PossrepComponentDef
                                  | PossrepComponentDefCommalistCons PossrepComponentDefCommalist PossrepComponentDef
    deriving (Show)

data PreviouslyDefinedStatementBody = PDSBAssignment Assignment
                                    | PDSBUserOpDef UserOpDef
                                    | PDSBUserOpDrop UserOpDrop
                                    | PDSBUserScalarTypeDef UserScalarTypeDef
                                    | PDSBUserScalarTypeDrop UserScalarTypeDrop
                                    | PDSBScalarVarDef ScalarVarDef
                                    | PDSBTupleVarDef TupleVarDef
                                    | PDSBRelationVarDef RelationVarDef
                                    | PDSBRelationVarDrop RelationVarDrop
                                    | PDSBConstraintDef ConstraintDef
                                    | PDSBConstraintDrop ConstraintDrop
                                    | PDSBArrayVarDef ArrayVarDef
                                    | PDSBRelationGet RelationGet
                                    | PDSBRelationSet RelationSet
    deriving (Show)

data ScalarTypeOrInitValue = ScalarTypeOrInitValueScalarTypeSpec ScalarTypeSpec
                           | ScalarTypeOrInitValueInit ScalarExp
                           | ScalarTypeOrInitValueScalarTypeSpecInit ScalarTypeSpec ScalarExp
    deriving (Show)

data TupleTypeOrInitValue = TupleTypeOrInitValueTupleTypeSpec TupleTypeSpec
                          | TupleTypeOrInitValueInit TupleExp
                          | TupleTypeOrInitValueTupleTypeSpecInit TupleTypeSpec TupleExp
    deriving (Show)

data Literal = Literal Identifier
    deriving (Show)

data RelationSet = RelationSet RelationTarget ArrayVarRef
    deriving (Show)

data RelationGet = RelationGet ArrayTarget RelationExp OrderItemCommalist
    deriving (Show)

data OrderItemCommalist = OrderItemCommalist OrderItem
                        | OrderItemCommalistCons OrderItemCommalist OrderItem
    deriving (Show)

data RelationVarDrop = RelationVarDrop RelationVarRef
    deriving (Show)

data RelationVarDef = RelationVarDefDatabase DatabaseRelationVarDef
                    | RelationVarDefApplication ApplicationRelationVarDef
    deriving (Show)

data TupleVarDef = TupleVarDef TupleVarName TupleTypeOrInitValue
    deriving (Show)

data ScalarVarDef = ScalarVarDef ScalarVarName ScalarTypeOrInitValue
    deriving (Show)

data UserScalarTypeDef = UserScalarTypeDef UserScalarRootTypeDef
    deriving (Show)

data UserScalarTypeDrop = UserScalarTypeDrop UserScalarTypeName
    deriving (Show)

data UserOpDef = UserOpDefUpdate UserUpdateOpDef
               | UserOpDefReadOnly UserReadOnlyOpDef
    deriving (Show)

data UserOpDrop = UserOpDrop UserOpName
    deriving (Show)

data UserUpdateOpDef = UserUpdateOpDef UserOpName ParameterDefCommalist ParameterNameCommalist Statement
                     | UserUpdateOpDefAllBut UserOpName ParameterDefCommalist ParameterNameCommalist Statement
    deriving (Show)

data UserReadOnlyOpDef = UserReadOnlyOpDef UserOpName ParameterDefCommalist TypeSpec Statement
    deriving (Show)

data ParameterDefCommalist = ParameterDefCommalist ParameterDef
                           | ParameterDefCommalistCons ParameterDefCommalist ParameterDef
    deriving (Show)

data UserScalarRootTypeDef = UserScalarRootTypeDef UserScalarTypeName PossrepDefList Literal
                           | UserScalarRootTypeDefOrdered UserScalarTypeName Ordering PossrepDefList Literal
    deriving (Show)

data PossrepDefList = PossrepDefList PossrepDef
                    | PossrepDefListCons PossrepDefList PossrepDef
    deriving (Show)

data RelationComp = RelationComp RelationExp RelationCompOp RelationExp
    deriving (Show)

data RelationCompOp = RelationCompOpEq
                    | RelationCompOpNe
    deriving (Show)

data ScalarComp = ScalarComp ScalarExp ScalarCompOp ScalarExp
    deriving (Show)

data ScalarCompOp = ScalarCompOpEq
                  | ScalarCompOpNe
    deriving (Show)

data SelectorInv = SelectorInvScalar ScalarSelectorInv
                 | SelectorInvNonscalar NonscalarSelectorInv
    deriving (Show)

data Summary = Summary SummarySpec
             | SummaryExp SummarySpec Exp
             | SummaryExpIntegerExpExp SummarySpec IntegerExp Exp
    deriving (Show)

data SummarySpec = SummarySpecCount
                 | SummarySpecCountd
                 | SummarySpecSum
                 | SummarySpecSumd
                 | SummarySpecAvg
                 | SummarySpecAvgd
                 | SummarySpecMax
                 | SummarySpecMin
                 | SummarySpecAnd
                 | SummarySpecOr
                 | SummarySpecXor
                 | SummarySpecExactly
                 | SummarySpecExactlyd
                 | SummarySpecUnion
                 | SummarySpecDunion
                 | SummarySpecIntersect
                 | SummarySpecXunion
    deriving (Show)

data TupleComp = TupleComp TupleExp TupleCompOp TupleExp
               -- TODO: add the other two later.
    deriving (Show)

data TupleCompOp = TupleCompOpEq
                 | TupleCompOpNe
    deriving (Show)

data VarRef = VarRefScalar ScalarVarRef
            | VarRefNonscalar NonscalarVarRef

}
