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
  ':='                  { \s -> AssignerTok      }

  begin                 { \s -> BeginTok         }
  transaction           { \s -> TransactionTok   }
  case                  { \s -> CaseTok          }
  end                   { \s -> EndTok           }
  else                  { \s -> ElseTok          }
  when                  { \s -> WhenTok          }
  then                  { \s -> ThenTok          }
  if                    { \s -> IfTok            }
  do                    { \s -> DoTok            }
  to                    { \s -> ToTok            }
  while                 { \s -> WhileTok         }
  leave                 { \s -> LeaveTok         }
  constraint            { \s -> ConstraintTok    }
  drop                  { \s -> DropTok          }
  return                { \s -> ReturnTok        }
  commit                { \s -> CommitTok        }
  rollback              { \s -> RollbackTok      }
  var                   { \s -> VarTok           }
  init                  { \s -> InitTok          }
  tuple                 { \s -> TupleTok         }
  from                  { \s -> FromTok          }
  all                   { \s -> AllTok           }
  but                   { \s -> ButTok           }
  union                 { \s -> UnionTok         }
  rename                { \s -> RenameTok        }
  extend                { \s -> ExtendTok        }
  wrap                  { \s -> WrapTok          }
  unwrap                { \s -> UnwrapTok        }
  as                    { \s -> AsTok            }
  compose               { \s -> ComposeTok       }
  heading               { \s -> HeadingTok       }
  d_union               { \s -> DunionTok        }
  intersect             { \s -> IntersectTok     }
  join                  { \s -> JoinTok          }
  times                 { \s -> TimesTok         }
  xunion                { \s -> XunionTok        }
  where                 { \s -> WhereTok         }
  group                 { \s -> GroupTok         }
  ungroup               { \s -> UngroupTok       }
  tclose                { \s -> TcloseTok        }
  minus                 { \s -> MinusTok         }
  i_minus               { \s -> IminusTok        }
  not                   { \s -> NotTok           }
  matching              { \s -> MatchingTok      }
  divideby              { \s -> DividebyTok      }
  summarize             { \s -> SummarizeTok     }
  per                   { \s -> PerTok           }
  by                    { \s -> ByTok            }
  prefix                { \s -> PrefixTok        }
  suffix                { \s -> SuffixTok        }
  key                   { \s -> KeyTok           }
  integer               { \s -> IntegerTok       }
  rational              { \s -> RationalTok      }
  character             { \s -> CharacterTok     }
  boolean               { \s -> BooleanTok       }
  real                  { \s -> RealTok          }
  base                  { \s -> BaseTok          }
  relation              { \s -> RelationTok      }
  with                  { \s -> WithTok          }
  table_dee             { \s -> TableDeeTok      }
  table_dum             { \s -> TableDumTok      }
  private               { \s -> PrivateTok       }
  public                { \s -> PublicTok        }
  count                 { \s -> CountTok         }
  array                 { \s -> ArrayTok         }
  update                { \s -> UpdateTok        }
  insert                { \s -> InsertTok        }
  delete                { \s -> DeleteTok        }
  d_insert              { \s -> DinsertTok       }
  i_delete              { \s -> IdeleteTok       }
  same_type_as          { \s -> SameTypeAsTok    }
  same_heading_as       { \s -> SameHeadingAsTok }
  true                  { \s -> TrueTok          }
  false                 { \s -> FalseTok         }

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
    AssignerTok      |
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
    ReturnTok        |
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

