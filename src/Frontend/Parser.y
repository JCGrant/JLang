{
module Frontend.Parser where
import Frontend.Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  int       { TokenInt $$ }
  var       { TokenSym $$ }
  '='       { TokenEq }
  '+'       { TokenPlus }
  '-'       { TokenMinus }
  '*'       { TokenTimes }
  '/'       { TokenDiv }
  '('       { TokenLParen }
  ')'       { TokenRParen }
  '\n'      { TokenNewLine }
  print     { TokenPrint }

%left '='
%left '+' '-'
%left '*' '/'
%left NEG

%%

Stmts
  : Stmt '\n' Stmts       { $1 : $3 }
-- Switching Stmt and Stmts around would be more efficient.
-- See https://www.haskell.org/happy/doc/html/sec-sequences.html
  | Stmt                  { [$1] }
  | {- empty -}           { [] }

Stmt
  : Expr                  { ExprStmt $1 }
  | var '=' Expr          { Assign $1 $3 }
  | print Expr            { Print $2 }

Expr
  : Expr '+' Expr         { Add $1 $3 }
  | Expr '-' Expr         { Sub $1 $3 }
  | Expr '*' Expr         { Mul $1 $3 }
  | Expr '/' Expr         { Div $1 $3 }
  | '(' Expr ')'          { $2 }
  | '-' Expr %prec NEG    { Neg $2 }
  | int                   { Int $1 }
  | var                   { Var $1 }

{

data Stmt
  = ExprStmt Expr
  | Assign String Expr
  | Print Expr
-- Will eventually make print a built-in function
  deriving (Eq, Show)

data Expr
  = Add Expr Expr
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