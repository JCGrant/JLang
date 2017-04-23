module Frontend.LexerSpec where

import Test.Hspec
import Frontend.Lexer

spec :: Spec
spec =
  describe "scanTokens" $ do
    it "does nothing on empty input" $
      scanTokens "" `shouldBe` []
    it "ignored whitespace" $
      scanTokens "   " `shouldBe` []
    it "ignored comments" $
      scanTokens "-- 123" `shouldBe` []
    it "scans ints" $ do
      scanTokens "1" `shouldBe` [TokenInt 1]
      scanTokens "23" `shouldBe` [TokenInt 23]
      scanTokens "456" `shouldBe` [TokenInt 456]
      scanTokens "0" `shouldBe` [TokenInt 0]
    it "scans =" $
      scanTokens "=" `shouldBe` [TokenEq]
    it "scans +" $
      scanTokens "+" `shouldBe` [TokenPlus]
    it "scans -" $
      scanTokens "-" `shouldBe` [TokenMinus]
    it "scans *" $
      scanTokens "*" `shouldBe` [TokenTimes]
    it "scans /" $
      scanTokens "/" `shouldBe` [TokenDiv]
    it "scans (" $
      scanTokens "(" `shouldBe` [TokenLParen]
    it "scans )" $
      scanTokens ")" `shouldBe` [TokenRParen]
    it "scans symbols" $ do
      scanTokens "x" `shouldBe` [TokenSym "x"]
      scanTokens "var" `shouldBe` [TokenSym "var"]
