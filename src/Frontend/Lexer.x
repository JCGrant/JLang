{
module Frontend.Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+                       ;
  "--".*                        ;
  $digit+                       { \s -> TokenInt (read s) }
  \=                            { \s -> TokenEq }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
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
  deriving (Eq, Show)

scanTokens = alexScanTokens
}