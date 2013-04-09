{
module Language.Heidi.Lexer where
}

%wrapper "basic"

$digit    = 0-9
$alpha    = [a-zA-Z]
$alphanum = $alpha # $digit
$ident    = [$alphanum _]

tokens :-
  $white+               ;
  "--".*                ;

  $digit+               { \s -> IntTok (read s) }

--  "\"".*"\""            { \s -> StrTok s        }
--  '{'                   { \s -> LeftCurlyTok    }
--  '}'                   { \s -> RightCurlyTok   }

  '\('                  { \s -> LeftRoundTok     }
  '\)'                  { \s -> RightRoundTok    }
  ':'                   { \s -> ColonTok         }
  ';'                   { \s -> SemiColonTok     }
  '\,'                  { \s -> CommaTok         }
  ":="                  { \s -> AssignerTok      }
  "="                   { \s -> EqualTok         }
  "<>"                  { \s -> NotEqualTok      }

  BEGIN                 { \s -> BeginTok         }
  TRANSACTION           { \s -> TransactionTok   }
  CASE                  { \s -> CaseTok          }
  END                   { \s -> EndTok           }
  ELSE                  { \s -> ElseTok          }
  CALL                  { \s -> CallTok          }
  WHEN                  { \s -> WhenTok          }
  THEN                  { \s -> ThenTok          }
  IF                    { \s -> IfTok            }
  DO                    { \s -> DoTok            }
  TO                    { \s -> ToTok            }
  WHILE                 { \s -> WhileTok         }
  LEAVE                 { \s -> LeaveTok         }
  CONSTRAINT            { \s -> ConstraintTok    }
  DROP                  { \s -> DropTok          }
  VIRTUAL               { \s -> VirtualTok       }
  ASC                   { \s -> AscTok           }
  DESC                  { \s -> DescTok          }
  ORDINAL               { \s -> OrdinalTok       }
  ORDERED               { \s -> OrderedTok       }
  POSSREP               { \s -> PossrepTok       }
  RETURN                { \s -> ReturnTok        }
  RETURNS               { \s -> ReturnsTok       }
  TYPE                  { \s -> TypeTok          }
  LOAD                  { \s -> LoadTok          }
  ORDER                 { \s -> OrderTok         }
  OPERATOR              { \s -> OperatorTok      }
  UPDATES               { \s -> UpdatesTok       }
  COMMIT                { \s -> CommitTok        }
  ROLLBACK              { \s -> RollbackTok      }
  VAR                   { \s -> VarTok           }
  INIT                  { \s -> InitTok          }
  TUPLE                 { \s -> TupleTok         }
  FROM                  { \s -> FromTok          }
  ALL                   { \s -> AllTok           }
  BUT                   { \s -> ButTok           }
  UNION                 { \s -> UnionTok         }
  RENAME                { \s -> RenameTok        }
  EXTEND                { \s -> ExtendTok        }
  WRAP                  { \s -> WrapTok          }
  UNWRAP                { \s -> UnwrapTok        }
  AS                    { \s -> AsTok            }
  COMPOSE               { \s -> ComposeTok       }
  HEADING               { \s -> HeadingTok       }
  D_UNION               { \s -> DunionTok        }
  INTERSECT             { \s -> IntersectTok     }
  JOIN                  { \s -> JoinTok          }
  TIMES                 { \s -> TimesTok         }
  XUNION                { \s -> XunionTok        }
  WHERE                 { \s -> WhereTok         }
  GROUP                 { \s -> GroupTok         }
  UNGROUP               { \s -> UngroupTok       }
  TCLOSE                { \s -> TcloseTok        }
  MINUS                 { \s -> MinusTok         }
  I_MINUS               { \s -> IminusTok        }
  NOT                   { \s -> NotTok           }
  MATCHING              { \s -> MatchingTok      }
  DIVIDEBY              { \s -> DividebyTok      }
  SUMMARIZE             { \s -> SummarizeTok     }
  PER                   { \s -> PerTok           }
  BY                    { \s -> ByTok            }
  PREFIX                { \s -> PrefixTok        }
  SUFFIX                { \s -> SuffixTok        }
  KEY                   { \s -> KeyTok           }
  INTEGER               { \s -> IntegerTok       }
  RATIONAL              { \s -> RationalTok      }
  CHARACTER             { \s -> CharacterTok     }
  BOOLEAN               { \s -> BooleanTok       }
  REAL                  { \s -> RealTok          }
  BASE                  { \s -> BaseTok          }
  RELATION              { \s -> RelationTok      }
  WITH                  { \s -> WithTok          }
  TABLE_DEE             { \s -> TableDeeTok      }
  TABLE_DUM             { \s -> TableDumTok      }
  PRIVATE               { \s -> PrivateTok       }
  PUBLIC                { \s -> PublicTok        }
  COUNT                 { \s -> CountTok         }
  ARRAY                 { \s -> ArrayTok         }
  UPDATE                { \s -> UpdateTok        }
  INSERT                { \s -> InsertTok        }
  DELETE                { \s -> DeleteTok        }
  I_DELETE              { \s -> IdeleteTok       }
  D_INSERT              { \s -> DinsertTok       }
  SAME_TYPE_AS          { \s -> SameTypeAsTok    }
  SAME_HEADING_AS       { \s -> SameHeadingAsTok }
  COUNTD                { \s -> CountdTok        }
  SUM                   { \s -> SumTok           }
  SUMD                  { \s -> SumdTok          }
  AVG                   { \s -> AvgTok           }
  AVGD                  { \s -> AvgdTok          }
  MAX                   { \s -> MaxTok           }
  MIN                   { \s -> MinTok           }
  AND                   { \s -> AndTok           }
  OR                    { \s -> OrTok            }
  XOR                   { \s -> XorTok           }
  EXACTLY               { \s -> ExactlyTok       }
  EXACTLYD              { \s -> ExactlydTok      }
  TRUE                  { \s -> TrueTok          }
  FALSE                 { \s -> FalseTok         }

  $ident+               { \s -> IdentTok s  }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    MatchingTok      |
    SummarizeTok     |
    DividebyTok      |
    NotTok           |
    IminusTok        |
    MinusTok         |
    TcloseTok        |
    GroupTok         |
    UngroupTok       |
    UnionTok         |
    RenameTok        |
    ExtendTok        |
    WrapTok          |
    UnwrapTok        |
    AsTok            |
    DunionTok        |
    JoinTok          |
    TimesTok         |
    WhereTok         |
    KeyTok           |
    PerTok           |
    ByTok            |
    ColonTok         |
    SemiColonTok     |
    CommaTok         |
    RightRoundTok    |
    LeftRoundTok     |
    SameTypeAsTok    |
    SameHeadingAsTok |
    CountdTok        |
    SumTok           |
    SumdTok          |
    AvgTok           |
    AvgdTok          |
    MinTok           |
    MaxTok           |
    AndTok           |
    OrTok            |
    XorTok           |
    ExactlyTok       |
    ExactlydTok      |
    AssignerTok      |
    EqualTok         |
    NotEqualTok      |
    InitTok          |
    TupleTok         |
    FromTok          |
    AllTok           |
    ButTok           |
    ComposeTok       |
    HeadingTok       |
    IntersectTok     |
    XunionTok        |
    PrefixTok        |
    SuffixTok        |
    IntegerTok       |
    RationalTok      |
    CharacterTok     |
    BooleanTok       |
    LeftCurlyTok     |
    RightCurlyTok    |
    TrueTok          |
    FalseTok         |
    PrivateTok       |
    PublicTok        |
    CountTok         |
    ArrayTok         |
    UpdateTok        |
    InsertTok        |
    DeleteTok        |
    DinsertTok       |
    IdeleteTok       |
    BeginTok         |
    TransactionTok   |
    CallTok          |
    CaseTok          |
    EndTok           |
    ElseTok          |
    WhenTok          |
    ThenTok          |
    IfTok            |
    DoTok            |
    ToTok            |
    WhileTok         |
    LeaveTok         |
    ConstraintTok    |
    DropTok          |
    VirtualTok       |
    AscTok           |
    DescTok          |
    OrdinalTok       |
    OrderedTok       |
    PossrepTok       |
    ReturnTok        |
    ReturnsTok       |
    TypeTok          |
    LoadTok          |
    OrderTok         |
    OperatorTok      |
    UpdatesTok       |
    CommitTok        |
    RollbackTok      |
    VarTok           |
    RealTok          |
    BaseTok          |
    WithTok          |
    RelationTok      |
    TableDeeTok      |
    TableDumTok      |
    IntTok Int       |
    StrTok String    |
    IdentTok String
    deriving (Eq, Show)

}

