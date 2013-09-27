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
sign = (char '-' >> return negate) <|> return id

intLiteral :: RPNParser ()
intLiteral = do
    int <- sign <*> (read <$> many1 digit)
    updateState (IntLit int:)

doubleLiteral :: RPNParser ()
doubleLiteral = do
    s <- sign
    intPart <- many1 digit
    char '.'
    fracPart <- many1 digit
    let double = s . read $ intPart ++ "." ++ fracPart
    updateState (DoubleLit double:)

-- We need to split out subtraction into its own parser so that we can
-- properly parse '-234' as IntLit (-234) and not as subtraction followed by
-- IntLit 234.
subOp :: RPNParser ()
subOp = string "-" >> binary (:-)

nonSubOp :: RPNParser ()
nonSubOp = foldr1 (<|>) $ map (\(op, ctor) -> string op >> binary ctor)
       [("+", (:+)),
        ("*", (:*)),
        ("/", (:/)),
        ("^", (:^))]

binary :: (AST -> AST -> AST) -> RPNParser ()
binary f = do
    stack <- getState
    case stack of
        (x:y:xs) -> setState $ f y x:xs
        _ -> fail "stack underflow on binary op"

rpn :: RPNParser AST
rpn = do
    many1 . lexeme $ nonSubOp <|> try doubleLiteral <|> try intLiteral <|> subOp
    eof
    stack <- getState
    case stack of
        [x] -> return x
        [] -> fail "empty stack at end"
        _ -> fail $
             "overfull stack of length " ++ show (length stack) ++ " at end"

parseRpn :: String -> Either ParseError AST
parseRpn = runParser rpn [] ""
