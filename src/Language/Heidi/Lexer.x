{
module Language.Heidi.Lexer where
}

%wrapper "basic"

$digit    = 0-9
$alpha    = [a-zA-Z]
$alphanum = $alpha # $digit
$ident    = [$alphanum _]

tokens :-
  $white+				;
  "--".*				;
  $digit+				{ \s -> IntTok (read s) }
  "\"".*"\""            { \s -> StrTok s }

  var                   { \s -> VarTok      }
  init                  { \s -> InitTok     }
  real                  { \s -> RealTok     }
  base                  { \s -> BaseTok     }
  relation              { \s -> RelationTok }
  with                  { \s -> WithTok     }
  table_dee             { \s -> TableDeeTok }
  table_dum             { \s -> TableDumTok }
  $ident+               { \s -> IdentTok s  }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
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
