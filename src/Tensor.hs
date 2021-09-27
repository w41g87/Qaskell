{-# LANGUAGE QuantifiedConstraints #-}
module Tensor where

{-
    The definition of tensor product should follow conventional tensor product properties.
-}

class (forall a. Monoid (t a)) => Tensor t where
    (|*|) :: t a -> t a -> t a
    x |*| y = x <> y
    norm :: Floating a => t a -> t a
    fromList :: Integral b => [a] -> b -> t a
    rank :: Integral b => t a -> b
    isSquare :: t a -> Bool