module Backend.InterpreterSpec where

import Test.Hspec
import qualified Data.Map as Map
import Frontend.Parser
import Backend.Interpreter

testEnv = Map.fromList [("x", Int 1), ("y", Int 2)]

spec :: Spec
spec =
  describe "eval" $ do
    it "evaluates ints" $
      eval (Int 1) testEnv `shouldBe` 1
    it "evaluates variables" $
      eval (Var "x") testEnv `shouldBe` 1
    it "evaluates binary arithmetic operations" $ do
      eval (Add (Int 1) (Int 2)) testEnv `shouldBe` 3
      eval (Sub (Int 4) (Int 3)) testEnv `shouldBe` 1
      eval (Mul (Int 3) (Int 2)) testEnv `shouldBe` 6
      eval (Div (Int 4) (Int 2)) testEnv `shouldBe` 2
      eval (Neg (Int 3)) testEnv `shouldBe` -3