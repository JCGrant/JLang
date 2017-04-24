module Frontend.ParserSpec where

import Test.Hspec
import Frontend.Lexer
import Frontend.Parser

spec :: Spec
spec =
  describe "parse" $ do
    it "parses ints" $
      parse [TokenInt 1] `shouldBe` [ExprStmt (Int 1)]
    it "parses symbols" $
      parse [TokenSym "x"] `shouldBe` [ExprStmt (Var "x")]
    it "parses assign statement" $
      parse [TokenSym "x", TokenEq, TokenInt 2] `shouldBe` [Assign "x" (Int 2)]
    it "parses assign statement" $
      parse [TokenSym "x", TokenEq, TokenInt 1, TokenPlus, TokenInt 2] `shouldBe` [Assign "x" (Add (Int 1) (Int 2))]
    it "parses addition" $
      parse [TokenInt 1, TokenPlus, TokenInt 2] `shouldBe` [ExprStmt (Add (Int 1) (Int 2))]
    it "parses subtraction" $
      parse [TokenInt 1, TokenMinus, TokenInt 2] `shouldBe` [ExprStmt (Sub (Int 1) (Int 2))]
    it "parses multiplication" $
      parse [TokenInt 1, TokenTimes, TokenInt 2] `shouldBe` [ExprStmt (Mul (Int 1) (Int 2))]
    it "parses division" $
      parse [TokenInt 1, TokenDiv, TokenInt 2] `shouldBe` [ExprStmt (Div (Int 1) (Int 2))]
    it "brackets simplify to whatever is inside them" $
      parse [TokenLParen, TokenInt 1, TokenRParen] `shouldBe` [ExprStmt (Int 1)]
    it "parses nagation" $
      parse [TokenMinus, TokenInt 1] `shouldBe` [ExprStmt (Neg (Int 1))]
    it "parses order of operations correctly" $ do
      parse (scanTokens "(2 + 3) * 4") `shouldBe` [ExprStmt (Mul (Add (Int 2) (Int 3)) (Int 4))]
      parse (scanTokens "(2 + 3) * -4 - 8 / 2") `shouldBe`
        [ExprStmt
          (Sub
            (Mul
              (Add
                (Int 2)
                (Int 3))
              (Neg (Int 4)))
            (Div
              (Int 8)
              (Int 2)))]
    it "parses an empty token stream as an empty list of statements" $
      parse [] `shouldBe` []
    it "parses multiple statements" $
      parse [
        TokenInt 1, TokenPlus, TokenInt 1, TokenNewLine,
        TokenInt 2, TokenPlus, TokenInt 2
      ] `shouldBe` [
        ExprStmt (Add (Int 1) (Int 1)),
        ExprStmt (Add (Int 2) (Int 2))
      ]
    it "parses print statements" $
      parse [TokenPrint, TokenInt 1] `shouldBe` [Print (Int 1)]
    it "parses a simple program" $
      parse (scanTokens "x = 1 + 1\nprint x") `shouldBe` [
        Assign "x" (Add (Int 1) (Int 1)),
        Print (Var "x")
      ]