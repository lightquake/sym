module Main where

import           Control.Applicative
import           Evaluate
import           Parser.AST
import           Parser.RPN
import           Pipes
import qualified Pipes.Prelude       as P
import           System.IO
import           Text.Parsec.Error   (errorMessages, messageString)

evaluatePrint :: Consumer String IO ()
evaluatePrint = do
    lift $ putStr "> " >> hFlush stdout
    str <- await
    lift $ case evaluate <$> parseRpn str of
            Left err -> print err
            Right (IntLit i) -> print i
            Right (DoubleLit d) -> print d
            Right ast -> print ast
    evaluatePrint

main :: IO ()
main = runEffect $ P.stdinLn >-> evaluatePrint
