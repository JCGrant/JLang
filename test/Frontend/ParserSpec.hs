module Frontend.ParserSpec where

import Test.Hspec
import Frontend.Lexer
import Frontend.Parser

spec :: Spec
spec =
  describe "parse" $ do
    it "parses ints" $
      parse [TokenInt 1] `shouldBe` Int 1
    it "parses symbols" $
      parse [TokenSym "x"] `shouldBe` Var "x"
    it "parses assigns" $
      parse [TokenSym "x", TokenEq, TokenInt 2] `shouldBe` Assign "x" (Int 2)
    it "parses addition" $
      parse [TokenInt 1, TokenPlus, TokenInt 2] `shouldBe` Add (Int 1) (Int 2)
    it "parses subtraction" $
      parse [TokenInt 1, TokenMinus, TokenInt 2] `shouldBe` Sub (Int 1) (Int 2)
    it "parses multiplication" $
      parse [TokenInt 1, TokenTimes, TokenInt 2] `shouldBe` Mul (Int 1) (Int 2)
    it "parses division" $
      parse [TokenInt 1, TokenDiv, TokenInt 2] `shouldBe` Div (Int 1) (Int 2)
    it "brackets simplify to whatever is inside them" $
      parse [TokenLParen, TokenInt 1, TokenRParen] `shouldBe` Int 1
    it "parses nagation" $
      parse [TokenMinus, TokenInt 1] `shouldBe` Neg (Int 1)
    it "parses order of operations correctly" $ do
      parse (scanTokens "(2 + 3) * 4") `shouldBe` Mul (Add (Int 2) (Int 3)) (Int 4)
      parse (scanTokens "(2 + 3) * -4 - 8 / 2") `shouldBe`
        Sub
          (Mul
            (Add
              (Int 2)
              (Int 3))
            (Neg (Int 4)))
          (Div
            (Int 8)
            (Int 2))