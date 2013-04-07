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

--  '('                   { \s -> LeftRoundTok    }
--  ')'                   { \s -> RightRoundTok   }
--  '{'                   { \s -> LeftCurlyTok    }
--  '}'                   { \s -> RightCurlyTok   }
--  '['                   { \s -> LeftSquareTok   }
--  ']'                   { \s -> RightSquareTok  }
  ':'                   { \s -> ColonTok        }
--  ','                   { \s -> CommaTok        }
  ':='                  { \s -> AssignerTok     }

  var                   { \s -> VarTok       }
  init                  { \s -> InitTok      }
  tuple                 { \s -> TupleTok     }
  from                  { \s -> FromTok      }
  all                   { \s -> AllTok       }
  but                   { \s -> ButTok       }
  union                 { \s -> UnionTok     }
  rename                { \s -> RenameTok    }
  extend                { \s -> ExtendTok    }
  wrap                  { \s -> WrapTok      }
  unwrap                { \s -> UnwrapTok    }
  as                    { \s -> AsTok        }
  compose               { \s -> ComposeTok   }
  heading               { \s -> HeadingTok   }
  d_union               { \s -> DunionTok    }
  intersect             { \s -> IntersectTok }
  join                  { \s -> JoinTok      }
  times                 { \s -> TimesTok     }
  xunion                { \s -> XunionTok    }
  where                 { \s -> WhereTok     }
  group                 { \s -> GroupTok     }
  ungroup               { \s -> UngroupTok   }
  tclose                { \s -> TcloseTok    }
  minus                 { \s -> MinusTok     }
  i_minus               { \s -> IminusTok    }
  not                   { \s -> NotTok       }
  matching              { \s -> MatchingTok  }
  divideby              { \s -> DividebyTok  }
  summarize             { \s -> SummarizeTok }
  per                   { \s -> PerTok       }
  by                    { \s -> ByTok        }
  prefix                { \s -> PrefixTok    }
  suffix                { \s -> SuffixTok    }
  key                   { \s -> KeyTok       }
  integer               { \s -> IntegerTok   }
  rational              { \s -> RationalTok  }
  character             { \s -> CharacterTok }
  boolean               { \s -> BooleanTok   }
  real                  { \s -> RealTok      }
  base                  { \s -> BaseTok      }
  relation              { \s -> RelationTok  }
  with                  { \s -> WithTok      }
  table_dee             { \s -> TableDeeTok  }
  table_dum             { \s -> TableDumTok  }
  true                  { \s -> TrueTok      }
  false                 { \s -> FalseTok     }

  $ident+               { \s -> IdentTok s  }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    MatchingTok   |
    SummarizeTok  |
    DividebyTok   |
    NotTok        |
    IminusTok     |
    MinusTok      |
    TcloseTok     |
    GroupTok      |
    UngroupTok    |
    UnionTok      |
    RenameTok     |
    ExtendTok     |
    WrapTok       |
    UnwrapTok     |
    AsTok         |
    DunionTok     |
    JoinTok       |
    TimesTok      |
    WhereTok      |
    KeyTok        |
    PerTok        |
    ByTok         |
    ColonTok      |
    AssignerTok   |
    InitTok       |
    TupleTok      |
    FromTok       |
    AllTok        |
    ButTok        |
    ComposeTok    |
    HeadingTok    |
    IntersectTok  |
    XunionTok     |
    PrefixTok     |
    SuffixTok     |
    IntegerTok    |
    RationalTok   |
    CharacterTok  |
    BooleanTok    |
    LeftCurlyTok  |
    RightCurlyTok |
    TrueTok       |
    FalseTok      |
    VarTok        |
    RealTok       |
    BaseTok       |
    WithTok       |
    RelationTok   |
    TableDeeTok   |
    TableDumTok   |
    IntTok Int    |
    StrTok String |
    IdentTok String
    deriving (Eq, Show)

}

