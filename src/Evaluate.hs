{-# LANGUAGE Rank2Types #-}
module Evaluate (
    evaluate
    ) where

import Parser.AST

-- Apply a binary operation, which can either apply on integers or doubles, to
-- an AST, evaluating the two branches along the way. The arguments should
-- satisfy
--
-- fromIntegral (f x y) = g (fromIntegral x) (fromIntegral y)
apply :: (Integer -> Integer -> Integer) ->
         (Double -> Double -> Double) ->
         (AST -> AST -> AST) ->
         AST -> AST -> AST
apply f g ctor l r = case (evaluate l, evaluate r) of
    (IntLit il, IntLit ir) -> IntLit $ f il ir
    (DoubleLit dl, DoubleLit dr) -> DoubleLit $ g dl dr
    (IntLit il, DoubleLit dr) -> DoubleLit $ g (fromIntegral il) dr
    (DoubleLit dl, IntLit ir) -> DoubleLit $ g dl (fromIntegral ir)
    (el, er) -> el `ctor` er

evaluate (IntLit i) = IntLit i
evaluate (DoubleLit d) = DoubleLit d
evaluate (l :+ r) = apply (+) (+) (:+) l r
evaluate (l :- r) = apply (-) (-) (:-) l r
evaluate (l :* r) = apply (*) (*) (:*) l r
evaluate (l :/ r) = apply div (/) (:/) l r
evaluate (l :^ r) = apply (^) (**) (:^) l r
