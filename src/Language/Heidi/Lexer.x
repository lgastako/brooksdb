{
module Language.Heidi.Lexer (main) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
  $white+				;
  "--".*				;
  $digit+				{ \s -> Int (read s) }

  var                   { \s -> Var }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    Var         |
    Int Int
    deriving (Eq, Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
