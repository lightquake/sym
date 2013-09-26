module Parser.RPNSpec where

import qualified Control.Monad as M
import           Parser.RPN
import           Test.Hspec

spec = describe "the RPN parser" $
       it "should exist" $ 1 `shouldBe` 1
