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
  $ident                { \s -> IdentTok s }
  "\"".*"\""            { \s -> StrTok s }
  
  var                   { \s -> VarTok }
  real                  { \s -> RealTok }
  relation              { \s -> RelationTok }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    VarTok           |
    RealTok          |
    RelationTok      |
    IntTok Int       |
    StrTok String    |
    IdentTok String 
    deriving (Eq, Show)

-- main = do
--   s <- getContents
--   print (alexScanTokens s)
-- 
}
