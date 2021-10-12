module State where

import Tensor
import Vector as V
import System.Posix.Internals (c_dup2)

data Multipartite t a = Entangled [Int] (t a) | Product (Multipartite t a) (Multipartite t a)

instance Semigroup (Multipartite t a) where
    (<>) = Product

instance (Tensor t, Num a) => Monoid (Multipartite t a) where
    mempty = Entangled mempty mempty

instance Tensor t => Num (Multipartite t a) where
    (+) = undefined
    (-) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Tensor t => Tensor (Multipartite t) where
    norm (Entangled is a) = Entangled is (norm a)
    norm (Product a b) = Product (norm a) (norm b)
    fromList l dim = Entangled [0..(rank q - 1)] q
        where q = fromList l dim
    rank (Entangled s _) = (fromIntegral . length) s
    rank (Product a b) = rank a + rank b
    isSquare (Entangled _ a) = isSquare a
    isSquare (Product a b) = isSquare a && isSquare b
