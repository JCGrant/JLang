module LibSpec where

import Test.Hspec
import Lib

spec :: Spec
spec =
  describe "f" $
    it "f 1 == 2" $
      f 1 `shouldBe` 2