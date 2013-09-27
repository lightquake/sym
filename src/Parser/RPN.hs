module Parser.RPN (
    parseRpn
    ) where

import Control.Applicative ((<$>), (<*>))
import Parser.AST
import Text.Parsec

type RPNParser = Parsec String [AST]

lexeme :: RPNParser a -> RPNParser a
lexeme p = do
    x <- p
    many $ char ' '
    return x

sign :: Num a => RPNParser (a -> a)
sign = (char '+' >> return id) <|> (char '-' >> return negate) <|> return id

intLiteral :: RPNParser ()
intLiteral = do
    int <- lexeme $ sign <*> (read <$> many1 digit)
    updateState (IntLit int:)

op :: RPNParser ()
op = char '+' >> binary (:+)

binary :: (AST -> AST -> AST) -> RPNParser ()
binary f = do
    stack <- getState
    case stack of
        (x:y:xs) -> setState $ f y x:xs
        _ -> fail "stack underflow on binary op"

rpn :: RPNParser AST
rpn = do
    many1 (intLiteral <|> op)
    stack <- getState
    case stack of
        [x] -> return x
        [] -> fail "empty stack at end"
        _ -> fail $
             "overfull stack of length " ++ show (length stack) ++ " at end"

parseRpn :: String -> Either ParseError AST
parseRpn = runParser rpn [] ""
