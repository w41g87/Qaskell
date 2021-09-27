module State where

import Tensor
import Vector as V

data State t a = Qubit (t a) | Product (State t a) (State t a)

instance Semigroup (State t a) where
    (<>) = Product

instance (Tensor t) => Monoid (State t a) where
    mempty = Qubit mempty

instance Tensor t => Tensor (State t) where
    norm (Qubit a) = Qubit $ norm a
    norm (Product a b) = Product (norm a) (norm b)
    fromList l dim = Qubit $ fromList l dim
    rank (Qubit a) = rank a
    rank (Product a b) = rank a + rank b
    isSquare (Qubit a) = isSquare a
    isSquare (Product a b) = isSquare a && isSquare b
