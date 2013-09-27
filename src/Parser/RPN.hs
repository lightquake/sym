module Parser.RPN (
    parseRpn
    ) where

import Parser.AST
import Text.Parsec

parseRpn :: String -> Either ParseError AST
parseRpn = undefined
