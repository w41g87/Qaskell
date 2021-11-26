module Multipartite where

import Tensor

data Multipartite t a = Entangled [Int] (t a) | Product (Multipartite t a) (Multipartite t a)

instance Semigroup (Multipartite t a) where
    (<>) = Product

instance (Tensor t a) => Monoid (Multipartite t a) where
    mempty = Entangled mempty mempty

instance (Tensor t a, Num (t a)) => Num (Multipartite t a) where
    Entangled xs a + Entangled ys b
        | xs == ys = Entangled xs (a + b)
        | otherwise = error "Multipartite addition: qubit index mismatch"
    (-) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

-- instance Tensor t => Tensor (Multipartite t) where
--     norm (Entangled is a) = Entangled is (norm a)
--     norm (Product a b) = Product (norm a) (norm b)
--     fromList l dim = Entangled [0..(rank q - 1)] q
--         where q = fromList l dim
--     rank (Entangled s _) = (fromIntegral . length) s
--     rank (Product a b) = rank a + rank b
--     isSquare (Entangled _ a) = isSquare a
--     isSquare (Product a b) = isSquare a && isSquare b

entangle :: Multipartite t a -> Multipartite t a
entangle (Entangled xs t) = Entangled xs t
entangle (Product (Entangled xs t0) (Entangled ys t1)) = undefined 

-- partitioning the qubits will save memory but seems to destroy any local phase information in the process
-- TODO: Phase-retaining partition function