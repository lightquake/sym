module Parser.RPNSpec where

import qualified Control.Monad as M
import           Parser.AST
import           Parser.RPN
import           Test.Hspec

shouldParseAs :: String -> AST -> Expectation
shouldParseAs str ast = case parseRpn str of
    Left err -> expectationFailure $
                "parsing " ++ str ++ ": \n" ++ show err
    Right ast' -> ast' `shouldBe` ast

shouldntParse str = parseRpn str `shouldSatisfy` isLeft where
  isLeft (Left _) = True
  isLeft _ = False

spec :: Spec
spec = do
    describe "successes" $ do
        it "should parse lone positive literals" $
            "2" `shouldParseAs` IntLit 2
        it "should parse simple integer expressions" $ do
            "2 2 +" `shouldParseAs` (IntLit 2 :+ IntLit 2)
            "10 20 *" `shouldParseAs` (IntLit 10 :* IntLit 20)
        it "should handle negative literals" $ do
            "-3" `shouldParseAs` IntLit (-3)
            "-4 7 -" `shouldParseAs` (IntLit (-4) :- IntLit 7)
            "-2.5" `shouldParseAs` DoubleLit (-2.5)
        it "should parse simple floating point expressions" $ do
            "1.5 3 /" `shouldParseAs` (DoubleLit 1.5 :/ IntLit 3)
            "2.2 3.3 ^" `shouldParseAs` (DoubleLit 2.2 :^ DoubleLit 3.3)
        it "should allow more than 2 numbers on the stack" $
            "1 2 3 4 + * -" `shouldParseAs`
                (IntLit 1 :- (IntLit 2 :* (IntLit 3 :+ IntLit 4)))
        it "should allow iterated operations" $
            "4 1 + 2 * 3 -" `shouldParseAs`
                (((IntLit 4 :+ IntLit 1) :* IntLit 2) :- IntLit 3)

    describe "failures" $ do
        it "should fail if the stack overflows at the end" $
            mapM_ shouldntParse ["2 2",
                                 "2 3 + 4",
                                 "1 2 3 4 - -"]
        it "should fail if the stack underflows" $
            mapM_ shouldntParse ["2 -",
                                 "99 99 - *",
                                 "^"]
