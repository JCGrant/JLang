{
module Frontend.Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+                       { \s -> checkWhiteSpace s }
  "--".*                        ;
  $digit+                       { \s -> TokenInt (read s) }
  \=                            { \s -> TokenEq }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  "print"                       { \s -> TokenPrint }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{
data Token
  = TokenInt Int
  | TokenSym String
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenLParen
  | TokenRParen
  | TokenNewLine
  | TokenWhiteSpace
  | TokenPrint
  deriving (Eq, Show)

checkWhiteSpace :: String -> Token
checkWhiteSpace s
  | '\n' `elem` s = TokenNewLine
  | otherwise     = TokenWhiteSpace

scanTokens = filter (/= TokenWhiteSpace) . alexScanTokens
}