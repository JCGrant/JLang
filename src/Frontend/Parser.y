{
module Frontend.Parser where
import Frontend.Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  int { TokenInt $$ }
  var { TokenSym $$ }
  '=' { TokenEq }
  '+' { TokenPlus }
  '-' { TokenMinus }
  '*' { TokenTimes }
  '/' { TokenDiv }
  '(' { TokenLParen }
  ')' { TokenRParen }

%left '='
%left '+' '-'
%left '*' '/'
%left NEG

%%

Expr
  : var '=' Expr          { Assign $1 $3 }
  | Expr '+' Expr         { Add $1 $3 }
  | Expr '-' Expr         { Sub $1 $3 }
  | Expr '*' Expr         { Mul $1 $3 }
  | Expr '/' Expr         { Div $1 $3 }
  | '(' Expr ')'          { $2 }
  | '-' Expr %prec NEG    { Neg $2 }
  | int                   { Int $1 }
  | var                   { Var $1 }

{
data Expr
  = Assign String Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  | Int Int
  | Var String
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}